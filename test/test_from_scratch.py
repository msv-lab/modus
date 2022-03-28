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


from modustest import Fact, ModusTestCase
from textwrap import dedent


class TestFromScratch(ModusTestCase):

    def test_nothing(self):
        mf = """a :- from("scratch")."""
        self.build(mf, "a")

    # For the following two tests, we use an alpine image as our final image, as
    # read_file need a shell and cat.

    def test_copy(self):
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

    def test_property(self):
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

    def test_run(self):
        mf = dedent("""\
            bbox :- from("busybox")::set_workdir("/bin").
            scratch_with_shell :-
                (
                    from("scratch"),
                    (
                        bbox::copy("sh", "sh"),
                        bbox::copy("cat", "cat")
                    )::in_workdir("/bin")
                )::append_path("/bin").
            a :- scratch_with_shell, run("echo hello > /aa").
        """)
        img = self.build(mf, "a")[Fact("a", ())]
        self.assertEqual(img.read_file("/aa"), "hello\n")

    def test_merge(self):
        mf = dedent("""\
            bbox :- from("busybox")::set_workdir("/bin").
            a :-
                (
                    from("scratch"),
                    bbox::copy("sh", "/bin/sh"),
                    bbox::copy("cp", "/bin/cp"),
                    (
                        bbox::copy("cat", "/bin/cat"),
                        run("echo hello > /aa")
                    )::merge
                )::append_path("/bin").
        """)
        img = self.build(mf, "a")[Fact("a", ())]
        self.assertEqual(img.read_file("/aa"), "hello\n")
