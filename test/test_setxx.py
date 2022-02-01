from modustest import ModusTestCase, Fact
from textwrap import dedent


class TestSetXX(ModusTestCase):
    def helper(self, mf, expect_result):
        mf = dedent(mf)
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertEqual(first_img.read_file("/result").strip(), expect_result)

    def test_set_env(self):
        self.helper("""\
        final :- from("alpine")::set_env("A", "1"),
                    run("echo $A > /result").""", "1")

    def test_set_env_2(self):
        # set_env changes the resulting image.
        self.helper("""\
        final :- (from("alpine"),
                    run("echo $A > /result"))::set_env("A", "1").""", "")

    def test_in_env(self):
        self.helper("""\
        final :- from("alpine"),
                    run("echo $A > /result")::in_env("A", "1").""", "1")

    def test_in_env_2(self):
        # in_env does not change image property.
        self.helper("""\
        final :- from("alpine"),
                    run("true")::in_env("A", "aaa"),
                    run("echo $A > /result").""", "")

    def test_append_path(self):
        mf = dedent("""\
        final :- from("alpine")::append_path("/appended_path"),
                    run("echo $PATH > /result").""")
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertTrue("/appended_path" in first_img.read_file("/result"))

    def test_append_path_2(self):
        # append_path modifies the resulting image.
        mf = dedent("""\
        final :- (from("alpine"),
                    run("echo $PATH > /result"))::append_path("/appended_path").""")
        imgs = self.build(mf, "final")
        self.assertEqual(len(imgs), 1)
        first_img = imgs[Fact("final", ())]
        self.assertTrue("/appended_path" not in first_img.read_file("/result"))
