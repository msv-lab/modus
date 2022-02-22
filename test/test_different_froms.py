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
