import json
import sys
import os

def agnosticise(j):
    fields = j['fields']
    placements = set()
    for p in j['placements']:
        names = frozenset(p['n'])
        data = frozenset(frozenset(zip(fields, f)) for f in p['p'])
        placements.add((names, data))
    return placements

def main(files):
    files = [(os.path.basename(f), agnosticise(json.load(open(f)))) for f in files]
    padding = max(max(len(f) for f, _ in files), 2)
    print ' ' * padding,
    for name, _ in files:
        print '%*s' % (padding, name),
    print
    compared = set()
    for e1, (name1, j1) in enumerate(files):
        print '%*s' % (padding, name1),
        for e2, (_, j2) in enumerate(files):
            pair = frozenset([e1, e2])
            if pair in compared or e1 == e2:
                print ' ' * padding,
                continue
            print '%*s' % (padding, '==' if j1 == j2 else '<>'),
            compared.add(pair)
        print

if __name__ == '__main__':
    main(sys.argv[1:])

