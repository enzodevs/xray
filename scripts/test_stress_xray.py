"""Unit checks for the stress harness (no synthetic source files)."""
import copy
import sys
import unittest

from stress_xray import run, validate_digest


class StressHarnessTests(unittest.TestCase):
    def setUp(self):
        self.digest = {'schema_version': 1, 'mode': 'digest', 'file': {
            'total_lines': 3, 'content': {'data': {'exports': [
                {'name': 'Widget', 'span': {'start_line': 1, 'end_line': 3}}
            ]}}}}

    def test_accepts_inclusive_final_line(self):
        validate_digest(self.digest)

    def test_rejects_phantom_trailing_line(self):
        bad = copy.deepcopy(self.digest)
        bad['file']['content']['data']['exports'][0]['span']['end_line'] = 4
        with self.assertRaises(AssertionError):
            validate_digest(bad)

    def test_rejects_empty_symbol_name(self):
        self.digest['file']['content']['data']['exports'][0]['name'] = ''
        with self.assertRaises(AssertionError):
            validate_digest(self.digest)

    def test_preserves_command_failure_and_stderr(self):
        code, expired, _, stdout, stderr = run(
            [sys.executable, '-c', 'import sys; print("failure", file=sys.stderr); sys.exit(7)'],
            '.', 5)
        self.assertEqual(code, 7)
        self.assertFalse(expired)
        self.assertEqual(stdout, b'')
        self.assertIn(b'failure', stderr)

    def test_deadline_terminates_blocked_command(self):
        code, expired, _, _, _ = run(
            [sys.executable, '-c', 'import threading; threading.Event().wait()'], '.', .1)
        self.assertTrue(expired)
        self.assertNotEqual(code, 0)


if __name__ == '__main__':
    unittest.main()
