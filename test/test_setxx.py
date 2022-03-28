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
