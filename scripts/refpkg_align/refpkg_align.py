#!/usr/bin/env python
import sys, os, string, argparse, re, subprocess
from string import Template
from Bio import SeqIO, AlignIO
from Bio.Seq import Seq, SeqRecord


def main():
    """
    Entry point for this script.
    """
    # Get command-line arguments.
    arguments = parse_arguments()
    out_prefix = arguments.out_prefix
    reference_package = arguments.refpkg[0]
    sequence_files = arguments.seqfiles

    # Create alignment with hmmer for all sequence files.
    hmmer_align(reference_package, sequence_files, out_prefix)


def hmmer_align(reference_package, sequence_files, out_prefix):
    """
    Create an alignment with hmmer. Then, separate out reference sequences 
    from the fragments into two separate files.
    """
  
    # Determine the name of the reference package, excluding any other 
    # elements in its path.
    reference_package_name = list(os.path.split(reference_package)).pop()
    reference_package_name_prefix = os.path.splitext(reference_package_name)[0]

    # Set default prefix if unspecified.
    out_prefix_arg = out_prefix
    if out_prefix is None:
        out_prefix = os.path.join(reference_package, reference_package_name_prefix) + '.' # Sequence file name will be appended within for loop.
        
        
    
    # hmmalign must be in PATH for this to work.

    command_regex = re.compile(r'\s+')
    hmmer_template = Template('hmmalign --outformat afa -o $tmp_file' + ' --mapali ' + \
                              '$reference_package/$reference_package_name_prefix' + \
                              '.ref.sth $reference_package/$reference_package_name_prefix' + \
                              '.ref.hmm $sequence_file')
    for sequence_file in sequence_files:
        
        # Determine a name for the temporary output file.
        tmp_file = sequence_file + '.' + str(os.getppid()) + '.afa'
        sequence_file_name = list(os.path.split(sequence_file)).pop()
        sequence_file_name_prefix = os.path.splitext(sequence_file_name)[0]

        hmmalign_command = hmmer_template.substitute(reference_package=reference_package, 
                                                     sequence_file=sequence_file,
                                                     reference_package_name_prefix=reference_package_name_prefix,
                                                     tmp_file=tmp_file,
                                                    )
        try:
            child = subprocess.Popen(hmmalign_command,
                                     stdin=None,
                                     stdout=None,
                                     stderr=None,
                                     shell=(sys.platform!="win32"))
            return_code = child.wait()
        
            # If return code was not 1, split off the reference sequences from the fragments.
            if not return_code:
                # Determine output file names.  Set to default if --outprefix was not specified.
                out_refs, out_frags = [os.path.join(reference_package, out_prefix + '.ref.fasta'), 
                                       os.path.join(reference_package, out_prefix + '.frag.fasta')]
                if not out_prefix_arg:
                    out_refs = out_prefix + sequence_file_name_prefix + ".ref.fasta"
                    out_frags = out_prefix + sequence_file_name_prefix + ".frag.fasta"
 
                frag_names = names(SeqIO.parse(sequence_file, "fasta"))
                in_seqs = SeqIO.parse(tmp_file, "fasta")
                SeqIO.write(id_filter(in_seqs, lambda(idstr): idstr not in frag_names), out_refs, "fasta")
                in_seqs = SeqIO.parse(tmp_file, "fasta")
                SeqIO.write(id_filter(in_seqs, lambda(idstr): idstr in frag_names), out_frags, "fasta")
        except:
            raise
        finally:
            os.remove(tmp_file)


def names(records):
    """
    Get the sequence names.
    """
    s = set()
    for record in records:
        s.add(record.id)
    return(s)


def id_filter(in_seqs, f):
    for record in in_seqs:
        if (f(record.id)):
            yield record

    

def parse_arguments():
    """
    Parse command-line arguments.
    """
    parser = argparse.ArgumentParser(description='refpkg_align - align sequence ' + \
                                                 'files to be incorporated in a reference package.')
    parser.add_argument('-o', '--outprefix', dest='out_prefix', help='Output file prefix. ' + \
                        'Defaults to refpkg_prefix.sequence_file_prefix ')
    parser.add_argument('refpkg', nargs=1, type=reference_package, help='Reference package directory.')
    parser.add_argument('seqfiles', nargs='+', help='A list of one or more fasta files.')
    return parser.parse_args()


def reference_package(reference_package):
    """
    A custom argparse 'type' to make sure the path to the reference package exists.
    """
    if os.path.isdir(reference_package):
        # Remove trailing / from directory if found.
        if reference_package.endswith('/'):
            reference_package = reference_package[:-1]
        return reference_package
    else:
        raise Exception, 'Path to reference package does not exist: ' + reference_package


    

if __name__ == '__main__':
    sys.exit(main())
