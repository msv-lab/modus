from modustest import ModusTestCase, Fact
from textwrap import dedent


class TestCopyWithin(ModusTestCase):
    def test_single_absolute(self):
        md = dedent("""\
            a :- from("alpine"), run("echo content > /tmp/file").
            b :- from("alpine"), a::copy("/tmp/file", "/tmp/copied").
            c :- from("alpine")::set_workdir("/tmp"), a::copy("/tmp/file", "copied").""")
        img_b = self.build(md, "b")[Fact("b", ())]
        img_c = self.build(md, "c")[Fact("c", ())]
        self.assertEqual(img_b.read_file("/tmp/copied"), "content\n")
        self.assertEqual(img_c.read_file("/tmp/copied"), "content\n")

    def test_single_relative(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp/"), run("echo content > /tmp/file").
            b :- from("alpine"), a::copy("file", "/tmp/copied").
            c :- from("alpine")::set_workdir("/tmp"), a::copy("file", "copied").""")
        img_b = self.build(md, "b")[Fact("b", ())]
        img_c = self.build(md, "c")[Fact("c", ())]
        self.assertEqual(img_b.read_file("/tmp/copied"), "content\n")
        self.assertEqual(img_c.read_file("/tmp/copied"), "content\n")

    def test_non_exist(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp").
            b :- from("alpine"), a::copy("/tmp/file", "/tmp/copied").
            c :- from("alpine"), a::copy("file", "/tmp/copied").""")
        self.build(md, "b", should_succeed=False)
        self.build(md, "c", should_succeed=False)

    def test_recursive(self):
        md = dedent("""\
            a :- from("alpine")::set_workdir("/tmp"), run("mkdir -p /tmp/dir/subdir && echo content > /tmp/dir/subdir/content").
            b :- from("alpine"), a::copy("/tmp/dir", "/tmp/copied").
            c :- from("alpine"), a::copy("dir", "/tmp/copied").""")
        img_b = self.build(md, "b")[Fact("b", ())]
        img_c = self.build(md, "c")[Fact("c", ())]
        self.assertEqual(img_b.read_file(
            "/tmp/copied/subdir/content"), "content\n")
        self.assertEqual(img_c.read_file(
            "/tmp/copied/subdir/content"), "content\n")

    def test_copy_chain(self):
        md = dedent("""\
            a :- from("alpine"), run("echo content > /tmp/file").
            b :- from("alpine"), a::copy("/tmp/file", "/tmp/bbb").
            c :- from("alpine"), b::copy("/tmp/bbb", "/tmp/ccc").
            d :- c, run("cp /tmp/ccc /tmp/ddd").
            f :- from("alpine"), (
                # e :-
                from("alpine"), d::copy("/tmp/ddd", "/tmp/eee")
            )::copy("/tmp/eee", "/tmp/fff").
        """)
        img_f = self.build(md, "f")[Fact("f", ())]
        self.assertEqual(img_f.read_file("/tmp/fff"), "content\n")

    def test_inworkdir(self):
        md = dedent("""\
            a :- from("alpine"), run("echo content > /tmp/file").
            b :-
                from("alpine")::set_workdir("/"),
                (
                    a::copy("/tmp/file", "bbb")
                )::in_workdir("/tmp").""")
        img_b = self.build(md, "b")[Fact("b", ())]
        self.assertEqual(img_b.read_file("/tmp/bbb"), "content\n")

    def test_multiple_copies(self):
        md = dedent("""\
            a :- from("alpine"), run("echo content > /tmp/file"), run("echo content2 > /tmp/file2").
            b :- from("alpine"), run("echo content3 > /tmp/file3").
            c :- from("alpine"),
                 a::copy("/tmp/file", "/tmp/copied"),
                 a::copy("/tmp/file2", "/tmp/copied2"),
                 b::copy("/tmp/file3", "/tmp/copied3").""")
        img_c = self.build(md, "c")[Fact("c", ())]
        self.assertEqual(img_c.read_file("/tmp/copied"), "content\n")
        self.assertEqual(img_c.read_file("/tmp/copied2"), "content2\n")
        self.assertEqual(img_c.read_file("/tmp/copied3"), "content3\n")

class TestCopyFromContext(ModusTestCase):
    def init_files(self):
        self.context.add_file("file", "content\n")
        self.context.add_file("dir/file", "content\n")

    def test_single_file(self):
        self.init_files()
        md = dedent("""\
            a :- from("alpine"), copy("file", "/tmp/file").
            b :- from("alpine")::set_workdir("/tmp"), copy("file", "file").""")
        img_a = self.build(md, "a")[Fact("a", ())]
        img_b = self.build(md, "b")[Fact("b", ())]
        self.assertEqual(img_a.read_file("/tmp/file"), "content\n")
        self.assertEqual(img_b.read_file("/tmp/file"), "content\n")

    def test_recursive(self):
        self.init_files()
        md = dedent("""\
            a :- from("alpine"), copy("dir", "/tmp/dir").
            b :- from("alpine")::set_workdir("/tmp"), copy("dir", "dir").""")
        img_a = self.build(md, "a")[Fact("a", ())]
        img_b = self.build(md, "b")[Fact("b", ())]
        self.assertEqual(img_a.read_file("/tmp/dir/file"), "content\n")
        self.assertEqual(img_b.read_file("/tmp/dir/file"), "content\n")

    def test_test_inworkdir(self):
        self.init_files()
        md = dedent("""\
            a :-
                from("alpine")::set_workdir("/"),
                copy("file", "file")::in_workdir("/tmp").""")
        img_a = self.build(md, "a")[Fact("a", ())]
        self.assertEqual(img_a.read_file("/tmp/file"), "content\n")

    def test_test_setworkdir(self):
        self.init_files()
        md = dedent("""\
            a :-
                from("alpine")::set_workdir("/tmp"),
                copy("file", "file").""")
        img_a = self.build(md, "a")[Fact("a", ())]
        self.assertEqual(img_a.read_file("/tmp/file"), "content\n")
