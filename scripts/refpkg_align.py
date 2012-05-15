#!/usr/bin/env python
import argparse
import contextlib
import itertools
import json
import logging
import os
import os.path
import re
import shlex
import subprocess
import sys
import tempfile

from Bio import SeqIO, AlignIO
from Bio.Seq import Seq

log = logging.getLogger(__name__)

class InvalidReferencePackage(Exception):
    pass

# Prefer taxtastic refpkg
try:
    from taxtastic.refpkg import Refpkg
except ImportError:
    class Refpkg(object):
        """
        Minimal representation of a reference package, supporting file lookup.

        All of this (and much more) is in taxtastic.refpkg, but this loses a
        dependency.
        """
        def __init__(self, path, create=False):
            if not os.path.isdir(path):
                raise InvalidReferencePackage("{0} is not a directory.".format(path))
            self.path = os.path.abspath(path)
            self.contents_path = self._join('CONTENTS.json')
            if not os.path.exists(self.contents_path):
                raise InvalidReferencePackage(
                        "CONTENTS.json not found. Is this a reference package?")

            with open(self.contents_path) as fp:
                self.contents = json.load(fp)

        def _join(self, *args):
            return os.path.join(self.path, *args)

        def resource_path(self, key):
            return self._join(self.contents['files'][key])

        def open_resource(self, key, *mode):
            return open(self.resource_path(key), *mode)


# Add some functionality to the Taxtastic / minimal reference package
# from above
class ReferencePackage(Refpkg):
    def __init__(self, *args, **kwargs):
        super(ReferencePackage, self).__init__(*args, **kwargs)

    def guess_align_method(self):
        if 'profile' not in self.contents['files']:
            return 'PyNAST'
        else:
            with self.open_resource('profile') as fp:
                header = next(fp)
            if header.startswith('INFERNAL-1'):
                return 'INFERNAL'
            elif header.startswith('HMMER3'):
                return 'HMMER3'
            else:
                raise ValueError(
                        "Couldn't determine alignment method from '{0}'".format(header))

    @property
    def has_mask(self):
        return 'mask' in self.contents['files']

@contextlib.contextmanager
def _temp_file(**kwargs):
    """
    Returns a handle for a temporary file, kept for the length of the context
    manager.
    """
    try:
        with tempfile.NamedTemporaryFile(delete=False, **kwargs) as tf:
            yield tf
    finally:
        if os.path.exists(tf.name):
            os.unlink(tf.name)


# Alignment tools
def _parse_stockholm_consensus(sto_handle):
    """
    Return a boolean list indicating if a column at a given index is a
    consensus column according to the GC RF line.
    """
    rf_rex = re.compile(r"#=GC RF\s+(\S*)\s*$")

    def is_consensus(c):
        if c not in 'x.':
            raise ValueError("Unexpected character in mask: {0}".format(c) +
                    " Only 'x' and '.' are permitted")
        return c == 'x'

    lines = (line.rstrip() for line in sto_handle)
    matches = (rf_rex.match(line) for line in lines)
    rf = "".join(m.group(1) for m in matches if m)
    return AlignmentMask([is_consensus(c) for c in rf])


class AlignmentMask(object):
    """
    A mask of positions which are revealed in the alignment.

    Any positions which evaluate to False in the mask are dropped.
    """

    def __init__(self, mask):
        self.mask = mask

    def __len__(self):
        return len(self.mask)

    def apply(self, maskable):
        """
        Apply the mask to a list/tuple/string of equal length
        """
        if not len(maskable) == len(self.mask):
            raise ValueError("Record length != mask length!")
        return list(itertools.compress(maskable, self.mask))

    def mask_records(self, sequence_records):
        """
        Apply mask to an iterable of SeqRecord objects, dropping positions
        evaluating to False in the records
        """
        log.info("Applying mask: keeping %d/%d positions", sum(self.mask), len(self))
        for record in sequence_records:
            if not len(record) == len(self.mask):
                raise ValueError("Record length != mask length!")
            seq = Seq(''.join(self.apply(str(record.seq))))

            # Do the same for letter annotations
            letter_annotations = {}
            for k, v in record.letter_annotations.items():
                new_value = self.apply(v)
                # Convert to string if it started as such, otherwise a list is
                # fine.
                if isinstance(v, basestring):
                    new_value = ''.join(new_value)
                letter_annotations[k] = new_value

            record.letter_annotations = {}
            record.seq = seq
            record.letter_annotations = letter_annotations
            yield record

    @property
    def unmasked_count(self):
        return sum(self.mask)

    @classmethod
    def from_csv_file(cls, handle, length):
        """
        Create an alignment mask from file handle containing a comma-delimited
        list of integers

        Each integer is a position which should *not* be masked in the output.
        """
        unmasked_positions = set(int(i.strip())
                                 for i in handle.read().split(','))
        mask = [i in unmasked_positions for i in xrange(length)]
        return cls(mask)

    def __and__(self, other):
        if not isinstance(AlignmentMask, other):
            raise TypeError("Cannot compare to " + str(other))

        new_mask = [i and j for i, j in zip(self.mask, other.mask)]
        return AlignmentMask(new_mask)

def generate_mask(refpkg, stockholm_alignment):
    """
    Generate an AlignmentMask from a reference package and stockholm alignment
    """
    with refpkg.open_resource('mask') as fp:
        unmasked_positions = set(int(i.strip())
                                     for i in fp.read().split(','))

    # Get length of alignment
    with open(stockholm_alignment) as fp:
        align_length = len(AlignIO.read(stockholm_alignment, 'stockholm')[0])

    # Load consensus columns
    with open(stockholm_alignment) as fp:
        consensus_columns = _parse_stockholm_consensus(fp)

    if not align_length == len(consensus_columns.mask):
        raise ValueError("Consensus Columns and Alignment have "
                "differing lengths")

    counter = itertools.count().next
    consensus_column_indexes = (counter() if i else None
                                for i in consensus_columns.mask)
    consensus_mask = AlignmentMask([i in unmasked_positions for i
                                    in consensus_column_indexes])
    return consensus_mask

def mask_sequences(refpkg, source, target):
    """
    Apply mask from reference package to the source file, writing to target file.
    """
    mask = generate_mask(refpkg, source)
    masked_sequences = mask.mask_records(
            SeqIO.parse(source, 'stockholm'))
    SeqIO.write(masked_sequences, target, 'stockholm')

def hmmer_align(refpkg, sequence_file, output_path, use_mask=True,
        use_mpi=False, mpi_args=None, mpi_program=None, program_path='hmmalign',
        alignment_options=None, stdout=None):
    d = os.path.dirname(output_path)
    with tempfile.NamedTemporaryFile(dir=d, prefix='.hmmer_aln') as tf:
        cmd = [program_path, '-o', tf.name,
                '--mapali', refpkg.resource_path('aln_sto')]
        cmd.extend(alignment_options or [])
        cmd.extend((refpkg.resource_path('profile'), sequence_file))
        log.info(' '.join(cmd))
        subprocess.check_call(cmd, stdout=stdout)

        if refpkg.has_mask and use_mask:
            mask_sequences(refpkg, tf.name, output_path)
        else:
            tf.delete = False
            tf.close()
            os.rename(tf.name, output_path)

def infernal_align(refpkg, sequence_file, output_path, use_mask=True,
        use_mpi=False, mpi_args=None, mpi_program='mpirun',
        program_path='cmalign', alignment_options=None, stdout=None):
    d = os.path.dirname(output_path)
    base_command = ['cmalign']
    if use_mpi:
        base_command = [mpi_program] + (mpi_args or []) + base_command + \
                       ['--mpi']

    with tempfile.NamedTemporaryFile(prefix='.infernal_aln', dir=d) as tf, \
         tempfile.NamedTemporaryFile(prefix='.infernal_merged', dir=d) as merged:
        cmd = base_command[:]
        cmd.extend(alignment_options or [])
        cmd.extend(['-o', tf.name, refpkg.resource_path('profile'),
                    sequence_file])
        log.info(' '.join(cmd))
        subprocess.check_call(cmd, stdout=stdout)

        # Merge
        log.info("Merging.")
        cmd = [program_path, '--merge', '-o', merged.name]
        cmd.extend(alignment_options or [])
        cmd.extend((refpkg.resource_path('profile'),
                    refpkg.resource_path('aln_sto'), tf.name))
        log.info(' '.join(cmd))

        # write the merge output to /dev/null: only the initial cmalign command
        # produces useful output.
        with open(os.devnull) as devnull:
            subprocess.check_call(cmd, stdout=devnull)
        tf.close()

        if refpkg.has_mask and use_mask:
            mask_sequences(refpkg, merged.name, output_path)
        else:
            merged.delete = False
            merged.close()
            os.rename(merged.name, output_path)

def pynast_align(refpkg, sequence_file, output_path, use_mask=True,
        use_mpi=False, mpi_args=None, mpi_program='mpirun',
        program_path='pynast', alignment_options=None, stdout=None):
    if use_mask and refpkg.has_mask:
        raise NotImplementedError("Cannot mask with PyNAST")

    cmd = [program_path]
    cmd.extend(alignment_options or [])
    cmd.extend(['-t', refpkg.resource_path('aln_fasta'),
                '-i', sequence_file,
                '-a', output_path])

    log.info(' '.join(cmd))
    subprocess.check_call(cmd, stdout=stdout)

ALIGNERS = {
    'HMMER3': hmmer_align,
    'INFERNAL': infernal_align,
    'PyNAST': pynast_align,
}

# Default options that can be used by scripts.
# Keys for search_options and alignment options must map to a valid profile.
ALIGNMENT_DEFAULTS = {
    'INFERNAL': ['-1', '--hbanded', '--sub', '--dna'],
    'PyNAST': ['-l', '150', '-f', os.devnull, '-g', os.devnull]
}

# Default output format
DEFAULT_FORMAT = {'HMMER3': 'stockholm', 'INFERNAL': 'stockholm', 'PyNAST': 'fasta'}

def align(arguments):
    """
    Align sequences to a reference package alignment.
    """
    refpkg = arguments.refpkg
    prof = arguments.profile_version or refpkg.guess_align_method()
    alignment_func = ALIGNERS[prof]
    alignment_options = (arguments.alignment_options or ALIGNMENT_DEFAULTS.get(prof))

    dn = os.path.dirname(arguments.outfile)
    with _temp_file(prefix='.refpkg_align', dir=dn) as tf:
        tf.close()
        r = alignment_func(refpkg, arguments.seqfile, tf.name,
            use_mask=arguments.use_mask, use_mpi=arguments.use_mpi,
            mpi_args=arguments.mpi_arguments, mpi_program=arguments.mpi_run,
            alignment_options=alignment_options, stdout=arguments.stdout)

        if (not arguments.output_format or
                arguments.output_format == DEFAULT_FORMAT[prof]):
            # No format converseion needed
            os.rename(tf.name, arguments.outfile)
        else:
            # Convert
            SeqIO.convert(tf.name, DEFAULT_FORMAT[prof], arguments.outfile,
                    arguments.output_format)

    return r

def extract(arguments):
    """
    Extract a reference alignment from a reference package
    """
    refpkg = arguments.refpkg

    # If not masking, just copy the sequences, reformatting if appropriate
    if not arguments.use_mask:
        with refpkg.open_resource('aln_sto') as input_fp:
            with arguments.output_file as output_fp:
                result = SeqIO.convert(input_fp, 'stockholm', output_fp,
                        arguments.output_format)
        logging.info("Wrote %d sequences", result)
        return

    # Mask will be applied if available
    with refpkg.open_resource('aln_sto') as fp:
        alignment_length = len(next(SeqIO.parse(fp, 'stockholm')))

        # Rewind
        fp.seek(0)
        sequences = SeqIO.parse(fp, 'stockholm')

        try:
            with refpkg.open_resource('mask') as fp:
                mask = AlignmentMask.from_csv_file(fp, alignment_length)
            logging.info("Applying mask - keeping %d/%d positions",
                    mask.unmasked_count, len(mask))
            sequences = mask.mask_records(sequences)
        except KeyError:
            log.warn("No mask found. Extracting all columns.")

        with arguments.output_file as output_fp:
            result = SeqIO.write(sequences, output_fp, arguments.output_format)
        logging.info("Wrote %d sequences.", result)


def main(argv=sys.argv[1:]):
    """
    Parse command-line arguments.
    """
    logging.basicConfig(level=logging.INFO,
            format="%(levelname)s: %(message)s")

    align_defaults = ' '.join('({0}: "{1}")'.format(profile, ' '.join(options)) for
            profile, options in
            ALIGNMENT_DEFAULTS.items())

    parser = argparse.ArgumentParser(description="""Wrapper
            script for Infernal and HMMER alignment binaries.""")

    # Setup sub-commands
    subparsers = parser.add_subparsers(dest='subparser_name')
    # Help
    parser_help = subparsers.add_parser('help', help='Help for actions')
    parser_help.add_argument('action', nargs=1)

    # align
    parser_align = subparsers.add_parser('align',
            help=align.__doc__)
    parser_align.set_defaults(func=align)
    parser_align.add_argument('--align-opts', dest='alignment_options',
            metavar='OPTS', help="""Alignment options, such as "--mapali
            $aln_sto".  '$' characters will need to be escaped if using
            template variables.  Available template variables are $aln_sto,
            $profile.  Defaults are as follows for the different profiles:
            """ + align_defaults, type=shlex.split)
    parser_align.add_argument('--alignment-method', dest='profile_version',
            choices=ALIGNERS.keys(), help="""Profile version to use.  [default:
            Guess. PyNAST is used if a valid CM or HMM is not found in the
            reference package.]""")
    parser_align.add_argument('--no-mask', default=True, dest="use_mask",
            action='store_false', help="""Do not
            trim the alignment to unmasked columns. [default:
            apply mask if it exists]""")
    parser_align.add_argument('refpkg', type=ReferencePackage, help="""Reference package
            directory""")
    parser_align.add_argument('seqfile', help="""Input file, in FASTA
            format.""")
    parser_align.add_argument('outfile', help="""Output file""")
    parser_align.add_argument('--stdout', help="""Write alignment program
            stdout to FILE [default: /dev/null]""", default=open(os.devnull),
            metavar='FILE', type=argparse.FileType('w'))
    parser_align.add_argument('--debug', action='store_true',
            help='Enable debug output', default=False)
    parser_align.add_argument('--verbose', action='store_true',
            help='Enable verbose output')
    parser_align.add_argument('--output-format', help="""Write output in FORMAT
            [default: stockholm for HMMER, INFERNAL; fasta for PyNAST]""",
            choices=('fasta', 'stockholm'))
    mpi_args = parser_align.add_argument_group(description="MPI Options [INFERNAL only]")
    mpi_args.add_argument('--use-mpi', action='store_true', default=False,
            help="""Use MPI [default: %(default)s]""")
    mpi_args.add_argument('--mpi-arguments', type=shlex.split,
            help="""Arguments to pass to mpirun [default: %(default)s]""")
    mpi_args.add_argument('--mpi-run', default='mpirun',
            help="""Name of mpirun executable [default: %(default)s]""")

    # extract
    parser_extract = subparsers.add_parser('extract',
            help=extract.__doc__)
    parser_extract.set_defaults(func=extract)
    parser_extract.add_argument("--output-format",
            default="stockholm", help="output format [default: %(default)s]")
    parser_extract.add_argument("--no-mask", dest='use_mask', default=True,
            action="store_false", help="""Do not apply mask to alignment
            [default: apply mask if it exists]""")
    parser_extract.add_argument('refpkg', type=ReferencePackage,
            help='Reference package directory')
    parser_extract.add_argument('output_file', type=argparse.FileType('w'),
            help="""Destination""")

    arguments = parser.parse_args()

    action = arguments.subparser_name
    arguments = parser.parse_args(argv)

    if action == 'help':
        main([str(arguments.action[0]), '-h'])

    arguments.func(arguments)

if __name__ == "__main__":
    main()
