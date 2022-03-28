# Modus, a language for building container images
# Copyright (C) 2022 University College London

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


class TestSimple(ModusTestCase):
    def test_1(self):
        mf = dedent("""\
          a :- from("alpine").
          b :- a, run("echo aaa > /tmp/a").""")
        imgs = self.build(mf, "b")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("b", ())]
        self.assertEqual(first_img.read_file("/tmp/a"), "aaa\n")

        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertFalse(first_img.contains_file("/tmp/a"))

    def test_1_indented(self):
        mf = """\
          a :- from("alpine").
          b :- a, run("echo aaa > /tmp/a")."""
        imgs = self.build(mf, "b")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("b", ())]
        self.assertEqual(first_img.read_file("/tmp/a"), "aaa\n")

        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertFalse(first_img.contains_file("/tmp/a"))

    def test_2(self):
        mf = dedent("""\
        a :- from("alpine")::set_workdir("/tmp/new_dir"),
             run("echo aaa > a").""")
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertEqual(first_img.read_file("/tmp/new_dir/a"), "aaa\n")

    def test_4(self):
        mf = dedent("""\
        a :- from("alpine")::set_workdir("/tmp/new_dir"),
                (run("echo aaa > a"))::in_workdir("bbb").""")
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertEqual(first_img.read_file("/tmp/new_dir/bbb/a"), "aaa\n")

    def test_5(self):
        mf = dedent("""\
        a :- from("alpine")::set_workdir("/tmp/\\n"),
                run("echo aaa > a").""")
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        # shell escape: \n is $'\n'
        self.assertEqual(first_img.read_file("$'/tmp/\\n/a'"), "aaa\n")

    def test_6(self):
        mf = dedent("""\
            a :- from("alpine")::set_workdir("/tmp/"),
                run("echo \\"# comment\\" > a").""")
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertEqual(first_img.read_file("/tmp/a"), "# comment\n")

    def test_workdir_interaction(self):
        md = dedent("""\
            a :-
                from("alpine")::set_workdir("/tmp"),
                run("pwd >> /log"),
                run("pwd >> /log")::in_workdir("a"),
                (
                    run("pwd >> /log"),
                    run("pwd >> /log")::in_workdir("c"),
                    run("pwd >> /log")::in_workdir("/")
                )::in_workdir("b").
        """)

        imgs = self.build(md, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/log"), dedent("""\
            /tmp
            /tmp/a
            /tmp/b
            /tmp/b/c
            /
        """))

    def test_run_failure(self):
        mf = """a :- from("alpine"), run("exit 1")."""
        self.build(mf, "a", should_succeed=False)

    def test_from_scratch_nothing(self):
        mf = """a :- from("scratch")."""
        self.build(mf, "a")

    # For the following two tests, we use an alpine image as our final image, as
    # read_file need a shell and cat.

    def test_from_scratch_copy(self):
        mf = dedent("""\
            a :- from("scratch"),
                 (
                     from("alpine"),
                     run("echo content > /aa")
                 )::copy("/aa", "/aa").
            b :- from("alpine"),
                 a::copy("/aa", "/aa").""")
        img = self.build(mf, "b")[Fact("b", ())]
        self.assertEqual(img.read_file("/aa"), "content\n")

        # just to make sure we can actually output a if we wanted to.
        self.build(mf, "a")

    def test_from_scratch_property(self):
        mf = dedent("""\
            a :- from("scratch")::set_workdir("/tmp"),
                 (
                     from("alpine"),
                     run("echo content > /aa")
                 )::copy("/aa", "aa").
            b :- from("alpine"),
                 a::copy("/tmp/aa", "/tmp/aa").""")
        img = self.build(mf, "b")[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/aa"), "content\n")

        # just to make sure we can actually output a if we wanted to.
        self.build(mf, "a")

    def test_many_outputs(self):
        mf = dedent("""\
            a(X) :-
            aa(X),
            from("alpine"),
            run("sleep 1"),
            run(f"echo ${X}").""") + "\n".join(f"aa(\"{i}\")." for i in range(1, 21))
        imgs = self.build(mf, "a(X)")
        self.assertEqual(len(imgs), 20)

    def test_label(self):
        mf = dedent("""\
            a :-
                from("alpine")
                    ::set_label("com.modus-continens.label-test", "hello").
            """)
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        img = imgs[Fact("a", ())]
        self.assertEqual(img.get_config()["Config"]["Labels"]["com.modus-continens.label-test"], "hello")

    def test_entrypoint(self):
        mf = dedent("""\
            a :-
                from("alpine")::set_entrypoint("/bin/echo").
            b :-
                from("alpine")::set_entrypoint(["/bin/echo", "hello"]).
            c(X) :-
                from("alpine")::set_entrypoint(["/bin/echo", X]).
            d(X) :-
                from("alpine")::set_entrypoint(X).
            """)
        def assert_ep_is(imgs, ep):
            self.assertEqual(len(imgs), 1)
            img = next(iter(imgs.values()))
            self.assertEqual(img.get_config()["Config"]["Entrypoint"], ep)
            self.assertIsNone(img.get_config()["Config"]["Cmd"])
        assert_ep_is(self.build(mf, "a"), ["/bin/echo"])
        assert_ep_is(self.build(mf, "b"), ["/bin/echo", "hello"])
        assert_ep_is(self.build(mf, 'c("aaa")'), ["/bin/echo", "aaa"])
        assert_ep_is(self.build(mf, 'd(["/bin/echo", "aaa"])'), ["/bin/echo", "aaa"])

    def test_cmd(self):
        mf = dedent("""\
            base :- from("alpine")::set_entrypoint("/bin/echo").
            a :-
                base::set_cmd([]).
            b :-
                base::set_cmd(["hello"]).
            c(X) :-
                base::set_cmd([X]).
            d(X) :-
                base::set_cmd(X).
            """)
        def assert_cmd_is(imgs, cmd):
            self.assertEqual(len(imgs), 1)
            img = next(iter(imgs.values()))
            self.assertEqual(img.get_config()["Config"]["Entrypoint"], ["/bin/echo"])
            self.assertEqual(img.get_config()["Config"]["Cmd"], cmd)
        assert_cmd_is(self.build(mf, "a"), [])
        assert_cmd_is(self.build(mf, "b"), ["hello"])
        assert_cmd_is(self.build(mf, 'c("aaa")'), ["aaa"])
        assert_cmd_is(self.build(mf, 'd(["aaa", "bbb"])'), ["aaa", "bbb"])
