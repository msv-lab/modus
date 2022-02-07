from modustest import ModusTestCase, Fact
from textwrap import dedent


class TestSolver(ModusTestCase):

    def test_mutiple_images(self):
        self.context.add_file("foo.txt", dedent("""\
            aaa
        """))
        modusfile = dedent("""\
            f(X) :-
                g(X),
                from("alpine"),
                copy("foo.txt", "/tmp/foo.txt"),
                run(f"echo ${X} >> /tmp/foo.txt").
            g("bbb").
            g("ccc").
        """)

        images = self.build(modusfile, 'f(X)')

        self.assertEqual(len(images), 2)
        bbb_image = images[Fact('f', ('bbb',))]
        self.assertEqual(bbb_image.read_file('/tmp/foo.txt'), 'aaa\nbbb\n')
        ccc_image = images[Fact("f", ("ccc",))]
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            ccc
        """))
