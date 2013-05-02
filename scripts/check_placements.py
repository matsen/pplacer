#!/usr/bin/env python
import logging
import json
import math
import sys

log = logging.getLogger(__name__)

def parse_placements(j):
    fields = j['fields']
    for p in j['placements']:
        if 'nm' in p:
            name = p['nm'][0][0]
        elif isinstance(p['n'], basestring):
            name = p['n']
        else:
            name = p['n'][0]
        data = [dict(zip(fields, f)) for f in p['p']]
        yield name, data

def approx_equal(f1, f2, epsilon=1e-4):
    return abs(f1 - f2) < epsilon

def check(fname):
    with open(fname) as infile:
        j = json.load(infile)
    has_post_prob = 'post_prob' in j['fields']
    for name, data in parse_placements(j):
        if has_post_prob:
            tot_pp = math.fsum(p['post_prob'] for p in data)
            if not approx_equal(1, tot_pp):
                if approx_equal(0, tot_pp):
                    log.critical("%s: post_prob sums to 0", name)
                else:
                    log.error("%s: post_prob doesn't sum to 1", name)
            if any(p['post_prob'] == 0 for p in data):
                log.warning("%s: some post_prob == 0", name)
        if not approx_equal(1, math.fsum(p['like_weight_ratio'] for p in data)):
            log.error("%s: like_weight_ratio doesn't sum to 1", name)
        if any(p['like_weight_ratio'] == 0 for p in data):
            log.warning("%s: some like_weight_ratio == 0", name)

if __name__ == '__main__':
    logging.basicConfig(stream=sys.stderr, level=logging.INFO)
    check(sys.argv[1])
