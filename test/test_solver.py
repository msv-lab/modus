# Modus, a language for building container images
# Copyright (C) 2022 ANONYMISED

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.

# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <https://www.gnu.org/licenses/>.


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
            is_windows(variant) :- variant = f"windows/${suffix}".

            app(X) :-
                (
                    is_windows(X),
                    from("jturolla/failing-container")
                ;
                    !is_windows(X),
                    from(X),
                    run("echo hello-world")
                ).
        """)

        images = self.build(md, 'app("alpine:3.15")', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("app", ("alpine:3.15",)), images)

    def test_supports_negation_in_queries(self):
        md = dedent("""\
            is_windows(variant) :- variant = f"windows/${suffix}".
            my_app(X) :- base_image(X), from(X), run("echo hello-world").

            base_image("windows/windowsservercore-ltsc2022").
            base_image("alpine:3.15").
        """)

        images = self.build(md, '!is_windows(X), my_app(X)', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("my_app", ("alpine:3.15",)), images)

    def test_supports_negation_and_anonymous_in_queries(self):
        md = dedent("""\
            is_windows(variant, suffix) :- variant = f"windows/${suffix}".
            my_app(X) :- base_image(X), from(X), run("echo hello-world").

            base_image("windows/windowsservercore-ltsc2022").
            base_image("alpine:3.15").
        """)

        images = self.build(md, '!is_windows(X, _), my_app(X)', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("my_app", ("alpine:3.15",)), images)

    def test_enforces_stratification(self):
        md = dedent("""\
            foo(X) :- bar(X).
            bar(X) :- !foo(X).
        """)

        images = self.build(md, 'foo(X)', should_succeed=False)

    def test_supports_nested_negation_binding(self):
        md = dedent("""\
            foo(X) :- !(t(X), !p(X)), a(X).
            a(X) :- from(X).
            p("alpine:3.15").
            t("alpine:3.15").
        """)

        images = self.build(md, 'foo("alpine:3.15")', should_succeed=True)
        self.assertEqual(len(images), 1)
        self.assertIn(Fact("foo", ("alpine:3.15",)), images)
