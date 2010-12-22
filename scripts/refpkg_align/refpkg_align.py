#!/usr/bin/env python
import sys, os, string, argparse, re
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

    # Create alignment with hammer for all sequence files.
    hammer_align(reference_package, sequence_files, out_prefix)


def hammer_align(reference_package, sequence_files, out_prefix):
    """
    """
  
    # Determine the name of the reference package, excluding any other 
    # elements in its path.
    reference_package_name = list(os.path.split(reference_package)).pop()

    # Set default prefix if unspecified.
    if out_prefix is None:
        out_prefix = reference_package + '.' # Sequence file name will be appended within for loop.
        
        
    
    # hmmalign must be in PATH for this to work.

    command_regex = re.compile(r'\s+')
    hmmer_template = Template('hmmalign --outformat afa -o $temp_file' + '.afa --mapali ' + \
                              '$reference_package/$reference_package_name' + \
                              '.sth $reference_package/$reference_package_name' + \
                              '.hmm $sequence_file')
    for sequence_file in sequence_files:
        # Determine a name for the temporary output file.
        temp_file = sequence_file + '.' + str(os.getppid())

        hmmalign_command = hmmer_template.substitute(reference_package=reference_package, 
                                                     sequence_file=sequence_file,
                                                     reference_package_name=reference_package_name,
                                                     temp_file=temp_file,
                                                    )

        print hmmalign_command
                                                     
   #     child = subprocess.Popen(hmmalign_command,
   #                              stdin=None,
   #                              stdout=None,
   #                              stderr=None,
   #                              shell=(sys.platform!="win32"))
   #     return_code = child.wait()
    

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


    


##refpkg["aln_fasta"]

# i would like to specify a refpkg and a set of sequences to align as positional arguments like
# refpkg_align COG0003.refpkg to_align.fasta
# an -o option provides an out prefix, which defaults to a concatenation of the refpkg name and the name of the sequences to align, so in this case
##out_prefix = "COG0003.to_align"

# say we have slurped the corresponding CONTENTS.json file into a dictionary called refpkg.

# step zero: find a temporary filename that we can write to safely. here we just use something dumb.
##aligned = "tmp.afa"

# step one: run hmmalign, which can be found in /mnt/silo/silo_researcher/Matsen_F/MatsenGrp/local/bin
# here we would run it like
#hmmalign --outformat afa -o tmp.afa --mapali COG0003.refpkg/COG0003.ref.sth COG0003.refpkg/COG0003.ref.hmm to_align.fasta 
# which in terms of the dictionary would be
#hmmalign --outformat afa -o tmp.afa --mapali refpkg["aln_sto"] refpkg["profile"] to_align.fasta 

# step two: split off the reference sequences from the fragments.
# hmmalign puts them together, which is good that they then appear in the same alignment frame, but bad in that we want them pulled into separate files
# here are the file names for the out files:
##out_refs = out_prefix+".ref.fasta"
##out_frags = out_prefix+".frag.fasta"

# here is a function for getting a set of ids
##def names(records):
##    """"get the sequence names."""
##    s = set()
##    for record in records:
##        s.add(record.id)
##    return(s)

##frag_names = names(SeqIO.parse("to_align.fasta", "fasta"))

# here is where we filter the seqs
##def id_filter(f):
##    for record in in_seqs:
##	if (f(record.id)):
##	    yield record

# this is not terribly elegant, in that we start from the beginning two times
# but i don't really mind.

# if we were using file objects rather than generator functions we could have 
# if (f(record.id)):
#     <write in out_frags>
# else:
#     <write in out_refs>

##in_seqs = SeqIO.parse(aligned, "fasta")
##SeqIO.write(id_filter(lambda(idstr): idstr not in frag_names), out_refs, "fasta")
##in_seqs = SeqIO.parse(aligned, "fasta")
##SeqIO.write(id_filter(lambda(idstr): idstr in frag_names), out_frags, "fasta")




if __name__ == '__main__':
    sys.exit(main())
