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

        ccc_image = images[Fact("f", ("alpine:3.14",))]
        self.assertTrue(ccc_image.contains_file("/tmp/foo.txt"))
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            alpine:3.14
        """))

        ccc_image = images[Fact("f", ("alpine:3.15",))]
        self.assertTrue(ccc_image.contains_file("/tmp/foo.txt"))
        self.assertEqual(ccc_image.read_file("/tmp/foo.txt"), dedent("""\
            aaa
            alpine:3.15
        """))

    def test_supports_logical_expressions(self):
        md = dedent("""\
            foo("1") :- from("alpine")::set_workdir("/tmp").
            """)

        self.build(md, 'X = "0", foo(X)', should_succeed=False)

        images = self.build(md, 'X = "1", foo(X)', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("foo", ("1",)), images)

    def test_supports_complicated_queries(self):
        md = dedent("""\
            foo(X) :-
                base_image(X),
                from(X).
            base_image("alpine:3.14").
            base_image("alpine:3.15").
        """)

        self.build(md, 'number_gt("3", version), foo(f"alpine:${version}")', should_succeed=False)

        images = self.build(md, 'number_gt(version, "3.14"), foo(f"alpine:${version}")', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("foo", ("alpine:3.15",)), images)

    def test_supports_negation(self):
        md = dedent("""\
            app(X) :-
                (
                    X = f"windows/${windows_variant}",
                    from("jturolla/failing-container")
                ;
                    X != f"windows/${windows_variant}",
                    from(X),
                    run("echo hello-world")
                ).
        """)

        images = self.build(md, 'app("alpine:3.15")', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("app", ("alpine:3.15",)), images)
