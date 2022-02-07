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
        # TODO: unimplemented
        return
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
