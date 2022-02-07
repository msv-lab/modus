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
        img = imgs[Fact("_query", ())]
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
        img = imgs[Fact("_query", ())]
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
        img = imgs[Fact("_query", ())]
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
        self.assertEqual(img.read_file("/tmp/ddd/dir/file"), "content\n")
