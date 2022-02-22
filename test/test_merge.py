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


class TestMerge(ModusTestCase):
    def init_files(self):
        self.context.add_file("file", "content\n")
        self.context.add_file("dir/file", "content\n")

    def test_simple_merge(self):
        mf = dedent("""\
            a :-
                from("alpine")::set_workdir("/tmp"),
                (
                    run("echo aaa > file"),
                    run("echo bbb > file"),
                    run("echo ccc > file2")
                )::merge.""")
        imgs = self.build(mf, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file"), "bbb\n")
        self.assertEqual(img.read_file("/tmp/file2"), "ccc\n")

    def test_merge_local_copy(self):
        self.init_files()
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    copy("file", "/tmp/file"),
                    run("echo newline >> /tmp/file")
                )::merge.""")
        imgs = self.build(mf, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file"), "content\nnewline\n")

    def test_merge_local_copy_inworkdir(self):
        self.init_files()
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    copy("file", "file"),
                    run("echo newline >> file")
                )::in_workdir("/tmp")::merge.""")
        imgs = self.build(mf, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file"), "content\nnewline\n")

    def test_merge_single_copy_inworkdir(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp/aaa"), run("echo aaaaa > /tmp/aaa/file").
            b :- from("alpine"), (
                a::copy("file", "aaa"),
                run("echo bbbbb >> aaa")
            )::in_workdir("/tmp")::merge.
        """)
        imgs = self.build(md, "b")
        img = imgs[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/aaa"), "aaaaa\nbbbbb\n")

    def test_merge_single_copy_setworkdir(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp/aaa"), run("echo aaaaa > /tmp/aaa/file").
            b :- from("alpine")::set_workdir("/tmp"), (
                a::copy("file", "aaa"),
                run("echo bbbbb >> aaa")
            )::merge.
        """)
        imgs = self.build(md, "b")
        img = imgs[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/aaa"), "aaaaa\nbbbbb\n")

    def test_merge_multi_copy(self):
        self.init_files()
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    (
                        from("alpine"),
                        run("echo aaa > /tmp/file")
                    )::copy("/tmp/file", "/tmp/file1"),
                    (
                        from("alpine"),
                        run("echo bbb > /tmp/file")
                    )::copy("/tmp/file", "/tmp/file2")
                )::merge.""")
        imgs = self.build(mf, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file1"), "aaa\n")
        self.assertEqual(img.read_file("/tmp/file2"), "bbb\n")

    def test_workdir_interaction(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp"), (
                run("pwd >> /log"),
                run("pwd >> /log")::in_workdir("a"),
                (
                    run("pwd >> /log"),
                    run("pwd >> /log")::in_workdir("c"),
                    run("pwd >> /log")::in_workdir("/")
                )::in_workdir("b")
            )::merge.""")

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
        mf = """a :- from("alpine"), run("exit 1")::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_run_failure_2(self):
        mf = """a :- from("alpine"), (run("true"), run("exit 1"), run("true"))::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_logic_preds_inside_merge(self):
        mf = dedent("""\
            a :- from("alpine"), (
                run("true"),
                "a" = "a",
                string_concat("a", "b", "ab")
            )::merge.""")
        self.build(mf, "a")


    def test_nested_merge(self):
        mf = dedent("""\
            a :-
                from("alpine")::set_workdir("/tmp"),
                (
                    run("echo aaa > file"),
                    (
                        run("echo bbb > file"),
                        run("echo ccc > file2")
                    )::merge,
                    run("echo ddd > file")
                )::merge.""")
        imgs = self.build(mf, "a")
        img = imgs[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file"), "ddd\n")
        self.assertEqual(img.read_file("/tmp/file2"), "ccc\n")

    def test_nop_merge(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    "1" = "1"
                )::merge.""")
        self.build(mf, "a")

    def test_copy_should_mkdir(self):
        self.init_files()
        mf = dedent("""\
            a :- from("alpine"), run("echo aaa > /tmp/aaa").
            b :-
                from("alpine"),
                (
                    a::copy("/tmp/aaa", "bbb/ccc"),
                    copy("file", "ddd/eee"),
                    copy("dir", "ddd")
                )::in_workdir("/tmp")::merge.
            """)
        imgs = self.build(mf, "b")
        img = imgs[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/bbb/ccc"), "aaa\n")
        self.assertEqual(img.read_file("/tmp/ddd/eee"), "content\n")
        self.assertEqual(img.read_file("/tmp/ddd/file"), "content\n")

    def test_copy_dir_content(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                run("mkdir /tmp/dir"),
                run("echo content > /tmp/dir/file").
            b :-
                from("alpine"),
                (
                    a::copy("/tmp/dir", "/tmp")
                )::merge.
        """)
        img = self.build(mf, "b")[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/file"), "content\n")

    def test_copy_dir_content_from_local(self):
        self.init_files()
        mf = dedent("""\
            a :-
                from("alpine"),
                (
                    copy("dir", "/tmp")
                )::merge.
        """)
        img = self.build(mf, "a")[Fact("a", ())]
        self.assertEqual(img.read_file("/tmp/file"), "content\n")

    def test_copy_empty_dir(self):
        mf = dedent("""\
            a :- from("alpine"), run("mkdir /tmp/dir").
            b :-
                from("alpine"),
                (
                    a::copy("/tmp/dir", "/tmp")
                )::merge,
                run("ls /tmp > /filelist").
        """)
        img = self.build(mf, "b")[Fact("b", ())]
        self.assertEqual(img.read_file("/filelist").strip(), "")

    def test_copy_hidden_files(self):
        mf = dedent("""\
            a :- from("alpine"), run("mkdir /tmp/dir"), run("echo content > /tmp/dir/.hidden").
            b :-
                from("alpine"),
                (
                    a::copy("/tmp/dir", "/tmp")
                )::merge.
        """)
        img = self.build(mf, "b")[Fact("b", ())]
        self.assertEqual(img.read_file("/tmp/.hidden"), "content\n")

    def test_copy_overwrite(self):
        md = dedent("""\
            a(X) :- from("alpine"), run(f"echo ${X} > /tmp/file").
            b :-
                from("alpine")::set_workdir("/tmp"),
                (
                    a("1")::copy("/tmp/file", "file"),
                    a("2")::copy("/tmp/file", "file")
                )::merge.
        """)
        img_b = self.build(md, "b")[Fact("b", ())]
        self.assertEqual(img_b.read_file("/tmp/file"), "2\n")

    def test_copy_overwrite_dir_content(self):
        md = dedent("""\
            a(X) :- from("alpine"), run(f"echo ${X} > /tmp/file").
            b :-
                from("alpine")::set_workdir("/tmp"),
                (
                    a("1")::copy("/tmp", "."),
                    a("2")::copy("/tmp", ".")
                ).
        """)
        img_b = self.build(md, "b")[Fact("b", ())]
        self.assertEqual(img_b.read_file("/tmp/file"), "2\n")

    def test_copy_failure(self):
        mf = """a :- from("alpine"), from("alpine")::copy("/aaa", "/aaa")::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_copy_failure_2(self):
        mf = """a :- from("alpine"), (run("true"), from("alpine")::copy("/aaa", "/aaa"), run("true"))::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_copy_local_failure(self):
        mf = """a :- from("alpine"), copy("aaa", "/aaa")::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_copy_local_failure_2(self):
        mf = """a :- from("alpine"), (run("true"), copy("aaa", "/aaa"), run("true"))::merge."""
        self.build(mf, "a", should_succeed=False)

    def test_merge_example(self):
        mf = dedent("""\
            image("merge") :-
                from("alpine")::set_workdir("/tmp"),
                inner::merge.

            inner :-
                run("dd if=/dev/urandom of=file bs=1M count=200"),
                run("sha1sum file > file.sha1"),
                run("rm file"),
                other_image::copy("file", "file2"),
                run("sha1sum file2 > file2.sha1"),
                run("rm file2").

            other_image :-
                from("debian")::set_workdir("/tmp"),
                run("dd if=/dev/urandom of=file bs=1M count=200").

            image("no merge") :-
                from("alpine")::set_workdir("/tmp"),
                inner.
        """)
        imgs = self.build(mf, "image(X)")
        merge_img = imgs[Fact("image", ("merge",))]
        no_merge_img = imgs[Fact("image", ("no merge",))]
        self.assertNotEqual(merge_img.read_file("/tmp/file.sha1"), no_merge_img.read_file("/tmp/file.sha1"))
        self.assertEqual(merge_img.read_file("/tmp/file2.sha1"), no_merge_img.read_file("/tmp/file2.sha1"))
