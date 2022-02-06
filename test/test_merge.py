from modustest import ModusTestCase, Fact
from textwrap import dedent


class TestMerge(ModusTestCase):
    def init_files(self):
        self.context.add_file("file", "content\n")
        self.context.add_file("dir/file", "content\n")

    def simple_merge(self):
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

    def merge_local_copy(self):
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
