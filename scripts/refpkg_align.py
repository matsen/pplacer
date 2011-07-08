#!/usr/bin/env python
import argparse
import itertools
import logging
import os
import os.path
import re
import shlex
import string
import subprocess
import sys
import tempfile

from Bio import SeqIO, AlignIO
from Bio.Seq import Seq
from taxtastic import refpkg


log = logging.getLogger(__name__)


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


class _Aligner(object):
    def __init__(self, refpkg, align_options='', use_mask=False):
        self.refpkg = refpkg
        template = string.Template(align_options)
        self.paths = dict((k, os.path.join(refpkg.path, v))
                           for k, v in refpkg.contents['files'].items())
        align_options = template.substitute(**self.paths)
        self.align_options = shlex.split(str(align_options))
        self.use_mask = use_mask

    def align(self, query_file, outfile):
        # run the alignment
        self.run_align(query_file, outfile)


class InfernalAligner(_Aligner):
    """
    Aligner which Infernal to align to reference package.
    """

    def run_align(self, query_file, outfile):
        with tempfile.NamedTemporaryFile(suffix='.sto', delete=False) as tf:
            query_temp = tf.name

        try:
            self._query_align(query_file, query_temp)
            self._merge(query_temp, outfile)
        finally:
            os.remove(query_temp)

    def _query_align(self, infile, outfile):
        cmd = ['cmalign']
        cmd.extend(self.align_options)
        cmd.extend(['-o', outfile, self.refpkg.resource_path('profile'),
            infile])
        log.info(' '.join(cmd))
        subprocess.check_call(cmd)


    def _merge(self, infile, outfile):
        cmd = ['cmalign', '--merge']
        cmd.extend(self.align_options)
        cmd.extend([ '-o', outfile, self.refpkg.resource_path('profile'),
            self.refpkg.resource_path('aln_sto'), infile])

        log.info(' '.join(cmd))
        subprocess.check_call(cmd)


class Hmmer3Aligner(_Aligner):
    """
    Aligner which HMMER to align to reference package.
    """

    @property
    def has_mask(self):
        return 'mask' in self.refpkg.contents['files']

    def _generate_masks(self, stockholm_alignment):
        if self.use_mask and self.has_mask:
            with self.refpkg.resource('mask') as fp:
                unmasked_positions = set(int(i.strip())
                                         for i in fp.read().split(','))

            # Get length of alignment
            with open(stockholm_alignment) as fp:
                align_length = len(AlignIO.read(stockholm_alignment, 'stockholm')[0])

            # Load consensus columns
            with open(stockholm_alignment) as fp:
                self.consensus_columns = _parse_stockholm_consensus(fp)

            if not align_length == len(self.consensus_columns.mask):
                raise ValueError("Consensus Columns and Alignment have "
                        "differing lengths")

            counter = itertools.count().next
            consensus_column_indexes = (counter() if i else None
                                for i in self.consensus_columns.mask)
            consensus_mask = AlignmentMask([i in unmasked_positions for i
                in consensus_column_indexes])
            return consensus_mask
        else:
            self.consensus_mask = None

    def run_align(self, query_file, outfile):
        # If a mask is defined and we're using it, create an intermediate file
        # for the unmasked results.
        # TODO: Should this be a tempfile, or is the full alignment useful?
        if self.has_mask and self.use_mask:
            bn, ext = os.path.splitext(outfile)
            unmasked = '.'.join((bn, 'unmasked', ext[1:]))
        else:
            unmasked = outfile

        cmd = ['hmmalign', '-o', unmasked]
        cmd.extend(self.align_options)
        cmd.append(self.refpkg.resource_path('profile'))
        cmd.append(query_file)

        # Run HMMalign
        log.info(' '.join(cmd))
        subprocess.check_call(cmd)

        # If a mask is defined and we're using it, apply the mask
        if self.has_mask and self.use_mask:
            # Mask to consensus
            mask = self._generate_masks(unmasked)
            log.info("Applying mask: %d/%d positions kept.",
                    mask.unmasked_count, len(mask))
            masked_sequences = mask.mask_records(
                    SeqIO.parse(unmasked, 'stockholm'))
            SeqIO.write(masked_sequences, outfile, 'stockholm')

    def search(self, query_file, outfile, search_opts):
        """
        Run hmmsearch
        """
        template = string.Template(search_opts)
        search_opts = template.substitute(**self.paths)
        search_opts = shlex.split(search_opts)
        cmd = ['hmmsearch']
        cmd.extend(search_opts)
        cmd.extend(['-A', outfile,
               self.refpkg.resource_path('profile'),
               query_file])

        log.info(' '.join(cmd))
        subprocess.check_call(cmd)


ALIGNERS = {
        'hmmer3': Hmmer3Aligner,
        'infernal1': InfernalAligner,
        'infernal1mpi': InfernalAligner,
}

SEARCH_SUPPORTED = 'hmmer3',

# Default options that can be used by scripts.
# Keys for search_options and alignment options must map to a valid profile.
ALIGNMENT_DEFAULTS = {
                        'min_length' : 1,
                        'profile' : 'hmmer3',
                        'search_options' : { 'hmmer3' : '--notextw --noali' },
                        'alignment_options' : { 'hmmer3' : '--mapali $aln_sto',
                                                'infernal1' : '-1 --hbanded '
                                                '--sub --dna',
                                              },
                        'sequence_file_format' : 'fasta',
                     }


def align(arguments):
    """
    Align sequences to a reference package alignment.
    """
    prof = arguments.profile_version
    aligner_class = ALIGNERS[prof]
    if not arguments.alignment_options:
        arguments.alignment_options = \
                ALIGNMENT_DEFAULTS['alignment_options'].get(prof, '')
    aligner = aligner_class(arguments.refpkg, arguments.alignment_options,
            arguments.use_mask)
    aligner.align(arguments.seqfile, arguments.outfile)


def search_align(arguments):
    """
    Search input sequences for matches to reference alignment, align
    high-scoring matches to reference.
    """
    prof = arguments.profile_version
    if not prof in SEARCH_SUPPORTED:
        raise argparse.ArgumentTypeError(("Sorry, {0} does not support "
                                          "search.").format(prof))

    aligner_class = ALIGNERS[prof]

    if not arguments.alignment_options:
        arguments.alignment_options = ALIGNMENT_DEFAULTS['alignment_options'][prof]
    if arguments.search_options:
        search_options = arguments.search_options
    else:
        search_options = ALIGNMENT_DEFAULTS['search_options'].get(prof, '')

    aligner = aligner_class(arguments.refpkg, arguments.alignment_options,
            arguments.use_mask)

    # Run search
    fd, search_output = tempfile.mkstemp(suffix='.sto', dir=os.getcwd())
    os.close(fd)
    try:
        aligner.search(arguments.seqfile, search_output, search_options)
        # Align
        aligner.align(search_output, arguments.outfile)
    finally:
        if not arguments.debug:
            log.debug("Removing %s", search_output)
            os.remove(search_output)


def main(argv=sys.argv[1:]):
    """
    Parse command-line arguments.
    """

    logging.basicConfig(level=logging.INFO)
    # Build up a list of search and align defaults, for help text used below.
    search_defaults = ''
    for profile in ALIGNMENT_DEFAULTS['search_options']:
        options =  ALIGNMENT_DEFAULTS['search_options'][profile]
        search_defaults += '(' + profile + ': "' + \
        options + '")'

    align_defaults = ' '.join('({0}: "{1}")'.format(profile, options) for
            profile, options in
            ALIGNMENT_DEFAULTS['alignment_options'].items())

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

    # search_align
    parser_searchalign = subparsers.add_parser('search-align',
                help=search_align.__doc__)
    parser_searchalign.set_defaults(func=search_align)

    parser_searchalign.add_argument('--search-opts', dest='search_options',
            metavar='OPTS', help="""search options, such as "--notextw --noali
            -E 1e-2" Defaults are as follows for the different profiles: """ +
            search_defaults)

    # With the exception of 'help', all subcommands share a certain
    # number of arguments, which are added here.
    for subcommand, subparser in subparsers.choices.items():
        if subcommand == 'help':
            continue

        subparser.add_argument('--align-opts', dest='alignment_options',
                metavar='OPTS', help="""Alignment options, such as "--mapali
                $aln_sto".  '$' characters will need to be escaped if using
                template variables.  Available template variables are $aln_sto,
                $profile.  Defaults are as follows for the different profiles:
                """ + align_defaults)
        subparser.add_argument('--profile-version', dest='profile_version',
                default=ALIGNMENT_DEFAULTS['profile'], choices=ALIGNERS.keys(),
                help='Profile version to use. [default: %(default)s]')
        subparser.add_argument('--use-mask', default=False,
                action='store_true', help="""If a mask exists for the refpkg,
                trim the alignment to unmasked columns. [default:
                %(default)s]""")
        #subparser.add_argument('--format', dest='sequence_file_format',
                #default=ALIGNMENT_DEFAULTS['sequence_file_format'],
                #help='Specify format of seqfile.  Default: %(default)s')
        subparser.add_argument('refpkg', type=reference_package,
                help='Reference package directory')
        subparser.add_argument('seqfile', help='A single fasta file')
        subparser.add_argument('outfile', help="""Output file""")
        subparser.add_argument('--debug', action='store_true',
                help='Enable debug output', default=False)
        subparser.add_argument('--verbose', action='store_true',
                help='Enable verbose output')

    arguments = parser.parse_args()

    action = arguments.subparser_name
    arguments = parser.parse_args(argv)

    if action == 'help':
        main([str(arguments.action[0]), '-h'])

    arguments.func(arguments)


def reference_package(reference_package):
    """
    A custom argparse 'type' to make sure the path to the reference package
    exists.
    """
    if os.path.isdir(reference_package):
        try:
            return refpkg.Refpkg(reference_package)
        except IOError:
            raise argparse.ArgumentTypeError("Invalid reference package.")
    else:
        raise argparse.ArgumentTypeError(
                'Path to reference package does not exist: ' +
                reference_package)

if __name__ == "__main__":
    main()
