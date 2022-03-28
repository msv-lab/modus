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


class TestSetXX(ModusTestCase):
    def helper(self, mf, expect_result):
        mf = dedent(mf)
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertEqual(first_img.read_file("/result").strip(), expect_result)
        return first_img

    def test_set_env(self):
        self.helper("""\
        final :- from("alpine")::set_env("A", "1"),
                    run("echo $A > /result").""", "1")

    def test_set_env_2(self):
        # set_env changes the resulting image.
        self.helper("""\
        final :- (from("alpine"),
                    run("echo $A > /result"))::set_env("A", "1").""", "")

    def test_in_env(self):
        self.helper("""\
        final :- from("alpine"),
                    run("echo $A > /result")::in_env("A", "1").""", "1")

    def test_in_env_2(self):
        # in_env does not change image property.
        img = self.helper("""\
        final :- from("alpine"),
                    run("true")::in_env("A", "aaa"),
                    run("echo $A > /result").""", "")
        envs = img.get_config()["Config"]["Env"]
        for e in envs:
            if e.startswith("A="):
                raise AssertionError("A found in env")

    def test_append_path(self):
        mf = dedent("""\
        final :- from("alpine")::append_path("/appended_path"),
                    run("echo $PATH > /result").""")
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertTrue("/appended_path" in first_img.read_file("/result"))

    def test_append_path_2(self):
        # append_path modifies the resulting image.
        mf = dedent("""\
        final :- (from("alpine"),
                    run("echo $PATH > /result"))::append_path("/appended_path").""")
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertTrue("/appended_path" not in first_img.read_file("/result"))

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
