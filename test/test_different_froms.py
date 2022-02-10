from modustest import Fact, ModusTestCase
from textwrap import dedent

class TestDifferentFroms(ModusTestCase):
    def test_different_froms(self):
        mf = dedent("""\
            a :-
                from("alpine"),
                run("true").
            b :-
                from("busybox"),
                run("true").
            r("a") :- a.
            r("b") :- b.
        """)
        imgs = self.build(mf, "r(X)")
        self.assertEqual(len(imgs), 2)
        a = imgs[Fact("r", ("a",))]
        b = imgs[Fact("r", ("b",))]
        self.assertTrue(a.contains_file("/etc/alpine-release"))
        self.assertTrue(not b.contains_file("/etc/alpine-release"))
