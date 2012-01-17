#!/usr/bin/env python

import argparse
import shutil
import json
import sys

replacements = {
    'tree_file': 'tree',
    'phylo_model_file': 'phylo_model',
}

def main(args):
    parser = argparse.ArgumentParser()
    parser.add_argument('infile', metavar='CONTENTS.json',
                        type=argparse.FileType('rb+'))
    args = parser.parse_args(args)

    shutil.copyfile(args.infile.name, args.infile.name + '.bak')
    contents = json.load(args.infile)
    for key in ['files', 'md5']:
        records = contents[key]
        for repl_in, repl_out in replacements.iteritems():
            if repl_in not in records:
                continue
            records[repl_out] = records.pop(repl_in)
    contents.setdefault('metadata', {})['format_version'] = '1.1'
    args.infile.seek(0)
    args.infile.truncate()
    json.dump(contents, args.infile, indent=2)

if __name__ == '__main__':
    main(sys.argv[1:])
