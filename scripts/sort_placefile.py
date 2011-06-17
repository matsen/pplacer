import operator
import argparse
import json
import sys

def main(args):
    parser = argparse.ArgumentParser()
    parser.add_argument(
        '-o', '--outfile', metavar='FILE',
        type=argparse.FileType('wb'), default=sys.stdout)
    parser.add_argument('infile', type=argparse.FileType('rb'))
    args = parser.parse_args(args)

    placerun = json.load(args.infile)
    placerun['placements'].sort(key=operator.itemgetter('n'))
    for placement in placerun['placements']:
        placement['p'].sort()
    json.dump(placerun, args.outfile, indent=2)

if __name__ == '__main__':
    main(sys.argv[1:])
