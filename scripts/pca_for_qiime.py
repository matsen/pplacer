#!/usr/bin/env python
import argparse
import csv

def main():
    parser = argparse.ArgumentParser(
        description=(
            "convert a `guppy pca` trans file to the format expected by "
            "QIIME's plotting functions"))
    parser.add_argument(
        'trans', type=argparse.FileType('r'), help="input PCA trans file")
    parser.add_argument(
        'tsv', type=argparse.FileType('w'), help="output QIIME-compatible TSV")

    args = parser.parse_args()

    with args.trans, args.tsv:
        reader = csv.reader(args.trans)
        writer = csv.writer(args.tsv, dialect=csv.excel_tab)
        first_line = next(reader)
        writer.writerow(['pc vector number'] + range(1, len(first_line)))
        writer.writerow(first_line)
        writer.writerows(reader)

if __name__ == '__main__':
    main()
