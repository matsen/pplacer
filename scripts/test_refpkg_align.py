"""
Unit tests for alignment module
"""

from cStringIO import StringIO
import os
import os.path
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

class TempFileTestCase(unittest.TestCase):
    def test_close(self):
        with refpkg_align._temp_file() as tf:
            self.assertFalse(tf.closed)
            tf.close()
            self.assertTrue(os.path.isfile(tf.name))
        self.assertFalse(os.path.exists(tf.name))

    def test_remove_ok(self):
        """Test that the file can be removed without error"""
        with refpkg_align._temp_file() as tf:
            tf.close()
            os.unlink(tf.name)

