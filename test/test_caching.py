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
