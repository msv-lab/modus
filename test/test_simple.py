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

    def test_3(self):
        mf = dedent("""\
        a :- (from("alpine")::set_workdir("/tmp/new_dir"),
                run("echo aaa > a"))::in_workdir("bbb").""")
        imgs = self.build(mf, "a")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("a", ())]
        self.assertEqual(first_img.read_file("/tmp/new_dir/bbb/a"), "aaa\n")

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
