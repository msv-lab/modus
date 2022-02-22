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


class TestCaching(ModusTestCase):
    def test_noredo(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                run("echo $RANDOM > /tmp/file").
            b("0") :- a.
            b("1") :- a.""")
        imgs = self.build(mf, "b(X)")
        self.assertEqual(len(imgs), 2)
        b0 = imgs[Fact("b", ("0",))]
        b1 = imgs[Fact("b", ("1",))]
        self.assertEqual(b0.read_file("/tmp/file"), b1.read_file("/tmp/file"))

    def test_subimage_noredo(self):
        mf = dedent("""\
            a(X) :-
                from("alpine"),
                run(f"echo ${X}"),
                other_image::copy("/tmp/rand", "/tmp/rand").
            b(X) :-
                from("alpine"),
                run(f"echo b ${X}"),
                other_image::copy("/tmp/rand", "/tmp/rand").
            other_image :-
                from("alpine"),
                run("dd if=/dev/urandom bs=100 count=1 | base64 > /tmp/rand").
            final("a", X) :- (X = "1"; X = "2"), a(X).
            final("b", X) :- (X = "1"; X = "2"), b(X).
        """)
        imgs = self.build(mf, "final(A, X)")
        self.assertEqual(len(imgs), 4)
        a1 = imgs[Fact("final", ("a", "1"))]
        a2 = imgs[Fact("final", ("a", "2"))]
        b1 = imgs[Fact("final", ("b", "1"))]
        b2 = imgs[Fact("final", ("b", "2"))]
        self.assertEqual(a1.read_file("/tmp/rand"), a2.read_file("/tmp/rand"))
        self.assertEqual(b1.read_file("/tmp/rand"), b2.read_file("/tmp/rand"))
        self.assertEqual(a1.read_file("/tmp/rand"), b1.read_file("/tmp/rand"))

    def test_merge_noredo(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    run("echo $RANDOM > /tmp/file"),
                    run("echo nop")
                )::merge.
            b("0") :- a.
            b("1") :- a.""")
        imgs = self.build(mf, "b(X)")
        self.assertEqual(len(imgs), 2)
        b0 = imgs[Fact("b", ("0",))]
        b1 = imgs[Fact("b", ("1",))]
        self.assertEqual(b0.read_file("/tmp/file"), b1.read_file("/tmp/file"))

    def test_merge_noredo_with_copy(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    run("echo $RANDOM > /tmp/file"),
                    (
                        from("alpine"),
                        run("echo aaaa > /tmp/aaa")
                    )::copy("/tmp/aaa", "/tmp/aaa")
                )::merge.
            b("0") :- a.
            b("1") :- a.""")
        imgs = self.build(mf, "b(X)")
        self.assertEqual(len(imgs), 2)
        b0 = imgs[Fact("b", ("0",))]
        b1 = imgs[Fact("b", ("1",))]
        self.assertEqual(b0.read_file("/tmp/file"), b1.read_file("/tmp/file"))
        self.assertEqual(b0.read_file("/tmp/aaa"), "aaaa\n")
        self.assertEqual(b1.read_file("/tmp/aaa"), "aaaa\n")
