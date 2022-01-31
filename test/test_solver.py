from modustest import ModusTestCase, Fact
from textdrap import dedent


class Test(modustest.TestCase):
    def test_1(self):
        file_content = dedent('''\
            aaa
            bbb
        ''')
        context = [ ("foo.txt", file_content) ]
        modusfile = dedent('''\
            a(X) :-
                from("alpine"),
                copy("foo.txt", "/tmp/foo.txt")
                run(f"echo ${X} >> /tmp/foo.txt")
        ''')
        images = self.build(modusfile, context, 'a("ccc")')
        self.assert(images[Fact("a", ["ccc"])].contains_file("/foo.txt"))
        expected_content = dedent('''\
            aaa
            bbb
            ccc
        ''')
        self.assertEqual(images['a("ccc")'].read_file("/tmp/foo.txt"), expected_content)
