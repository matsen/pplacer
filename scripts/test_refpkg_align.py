"""
Unit tests for alignment module
"""

from cStringIO import StringIO
import unittest

from Bio.Seq import Seq
from Bio.SeqRecord import SeqRecord

import refpkg_align

class ParseStockholmConsensus(unittest.TestCase):

    def setUp(self):
        self.value = """
#=GC RF                      xxx.xx
#=GC RF                      xx.xxx
#=GC RF                      xxx.x.
#=GF TT                      IGNORED
"""

        self.expected = [True, True, True, False, True, True,
                        True, True, False, True, True, True,
                        True, True, True, False, True, False]
        self.handle = StringIO(self.value)

    def _get_result(self):
        return refpkg_align._parse_stockholm_consensus(self.handle).mask

    def test_length(self):
        result = self._get_result()
        self.assertEqual(len(self.expected), len(result))

    def test_value(self):
        self.assertEqual(self.expected, self._get_result())

    def test_no_other_chars(self):
        handle = StringIO('#=GC RF                      xxxCx.x.\n')
        self.assertRaises(ValueError, refpkg_align._parse_stockholm_consensus,
                handle)


class AlignmentMaskTestCase(unittest.TestCase):

    def setUp(self):
        self.sequences = [
                SeqRecord(Seq('ACGTACGT')),
                SeqRecord(Seq('AAAAAAAT')),
                SeqRecord(Seq('ACGTAAAA')),
                ]

    def test_invalid_seq_length(self):
        mask = [True, True, False, True]
        instance = refpkg_align.AlignmentMask(mask)
        self.assertRaises(ValueError,
                instance.mask_records(self.sequences).next)

    def test_mask(self):
        mask = [True, False, True, True, False, False, False, True]
        instance = refpkg_align.AlignmentMask(mask)
        actual = list(instance.mask_records(self.sequences))
        # equality isn't defined for SeqRecord, so this is basic
        actual = [str(r.seq) for r in actual]
        expected = ['AGTT', 'AAAT', 'AGTA']
        self.assertEqual(expected, actual)

    def test_from_csv_file(self):
        value = """
        1, 2,5,
        8,  10,
         11

        """
        handle = StringIO(value)
        expected = [False, True, True, False, False, True,
                False, False, True, False, True, True, False]

        actual = refpkg_align.AlignmentMask.from_csv_file(handle, 13)
        self.assertEqual(expected, actual.mask)

    def test_len(self):
        mask = [True, True, False, True]
        instance = refpkg_align.AlignmentMask(mask)
        expected = 4
        self.assertEqual(expected, len(instance))

    def test_unmasked_count(self):
        mask = [True, True, False, True, False]
        instance = refpkg_align.AlignmentMask(mask)
        expected = 3
        self.assertEqual(expected, instance.unmasked_count)

class MockSubprocess(object):
    """
    Mock of the subprocess module to support Check Call
    """
    def __init__(self):
        self.commands = []
        pass

    def check_call(self, cmd, *args, **kwargs):
        self.commands.append(cmd)

class MockRefpkg(object):

    def __init__(self):
        self.contents = {'files': {}}

    def resource_path(self, path):
        return 'resource-' + path

class SubprocessMixIn(object):
    """
    Converts the subprocess.check_call function to a mock for the test
    """

    def setUp(self):
        self._old_subproc = refpkg_align.subprocess
        refpkg_align.subprocess = MockSubprocess()

    def tearDown(self):
        refpkg_align.subprocess = self._old_subproc

    @property
    def latest_command(self):
        return refpkg_align.subprocess.commands[-1]

class InfernalAlignerTestCase(SubprocessMixIn, unittest.TestCase):
    """
    Some simple tests
    """

    def setUp(self):
        super(InfernalAlignerTestCase, self).setUp()
        self.instance = refpkg_align.InfernalAligner(MockRefpkg(), '--test')

    def test_query_align(self):
        self.instance._query_align('infile', 'outfile')
        self.assertEqual(['cmalign', '--test', '-o', 'outfile',
            'resource-profile', 'infile'], self.latest_command)

    def test_merge(self):
        self.instance._merge('infile', 'outfile')
        expected = ['cmalign', '--merge', '--test', '-o', 'outfile',
                'resource-profile', 'resource-aln_sto', 'infile']

        self.assertEqual(expected, self.latest_command)

class Hmmer3AlignerTestCase(SubprocessMixIn, unittest.TestCase):

    def setUp(self):
        super(Hmmer3AlignerTestCase, self).setUp()
        self.instance = refpkg_align.Hmmer3Aligner(MockRefpkg(), '--test')

    # need some tests

    def test_run_align(self):
        self.instance.run_align('infile', 'outfile')
        self.assertEqual(['hmmalign', '-o', 'outfile', '--test',
            'resource-profile', 'infile'], self.latest_command)

    def test_search(self):
        self.instance.search('infile', 'outfile', '--test1 A -h')
        self.assertEqual(['hmmsearch', '--test1', 'A', '-h', '-A', 'outfile',
                           'resource-profile', 'infile'],
                           self.latest_command)

