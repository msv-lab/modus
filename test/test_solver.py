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
        bbb_image = images[Fact('_query', ('bbb',))]
        self.assertEqual(bbb_image.read_file('/tmp/foo.txt'), 'aaa\nbbb\n')
        ccc_image = images[Fact("_query", ("ccc",))]
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            ccc
        """))

    def test_supports_modus_terms(self):
        self.context.add_file("foo.txt", dedent("""\
            aaa
        """))
        modusfile = dedent("""\
            f(X) :-
                base_image(X),
                from(X),
                copy("foo.txt", "/tmp/foo.txt"),
                run(f"echo ${X} >> /tmp/foo.txt").
            base_image("alpine:3.14").
            base_image("alpine:3.15").
        """)

        images = self.build(modusfile, 'f(f"alpine:${version}")')

        self.assertEqual(len(images), 2, images)

        ccc_image = images[Fact("_query", ("alpine:3.14",))]
        self.assertTrue(ccc_image.contains_file("/tmp/foo.txt"))
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            alpine:3.14
        """))

        ccc_image = images[Fact("_query", ("alpine:3.15",))]
        self.assertTrue(ccc_image.contains_file("/tmp/foo.txt"))
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            alpine:3.15
        """))
