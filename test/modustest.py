import unittest
from collections import namedtuple


Point = namedtuple("Fact", "predicate args")


class Image:

    def contains_file(self, path):
        pass

    def read_file(self, path):
        pass


class ModusTestCase(unittest.TestCase):

    def build(self, modusfile, context, query):
        '''returns a mapping from facts to images'''
        pass
