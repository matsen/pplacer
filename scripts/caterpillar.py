import collections
import subprocess
import argparse
import tempfile
import json
import csv
import sys

def main(args):
    parser = argparse.ArgumentParser()
    parser.add_argument('-o', '--outfile', metavar='FILE', type=argparse.FileType('ab'))
    parser.add_argument('-n', '--n-leaves', metavar='COUNT', type=int, default=20)
    parser.add_argument('-t', '--tau', type=float, default=2)
    parser.add_argument('--rppr', default='rppr')
    parser.add_argument('--gencaterpillar', default='gencaterpillar')
    args = parser.parse_args(args)

    base_placefile = tempfile.NamedTemporaryFile().name
    placefile = base_placefile + '.jplace'
    subprocess.check_call([args.gencaterpillar, str(args.tau), str(args.n_leaves), base_placefile])
    with open(placefile, 'rb') as fobj:
        tree = json.load(fobj)['tree']
    logfile = tempfile.NamedTemporaryFile().name
    subprocess.check_call(
        [args.rppr, 'voronoi',
         '--leaf-mass', '1', placefile, '--log', logfile, '--leaves', str(args.n_leaves)])
    with open(logfile, 'rb') as fobj:
        counter = collections.Counter()
        for node, _, _, _, _, _ in csv.reader(fobj):
            counter[node] += 1
        (_, solutions), = counter.most_common(1)
    csv.writer(args.outfile).writerow([args.n_leaves, solutions, tree])

if __name__ == '__main__':
    main(sys.argv[1:])
