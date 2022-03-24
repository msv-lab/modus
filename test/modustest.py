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


import os
import contextlib
from subprocess import run, DEVNULL, PIPE
from pathlib import Path
import unittest
from collections import namedtuple
from tempfile import TemporaryDirectory, NamedTemporaryFile
import json
from io import StringIO


MODUS_EXECUTABLE = "modus" if os.getenv('MODUS_EXECUTABLE') is None else os.getenv('MODUS_EXECUTABLE')
MODUS_BUILDKIT_FRONTEND = os.getenv('MODUS_BUILDKIT_FRONTEND')  # None means not using custom frontend version


Fact = namedtuple("Fact", "predicate args")


@contextlib.contextmanager
def cd(path):
   """Changes working directory and returns to previous on exit."""
   old_path = Path.cwd()
   os.chdir(path)
   try:
       yield
   finally:
       os.chdir(old_path)


class Image:

    def __init__(self, digest):
        self.digest = digest

    def contains_file(self, path):
        cmd = ["docker", "run", self.digest, "/bin/sh", "-c", f"test -f {path}"]
        result = run(cmd)
        return result.returncode == 0

    def read_file(self, path):
        cmd = ["docker", "run", self.digest, "/bin/sh", "-c", f"cat {path}"]
        result = run(cmd, check=True, text=True, stdout=PIPE, stderr=DEVNULL)
        return result.stdout

class Context(TemporaryDirectory):

    def add_file(self, path, content):
        dirname = Path(self.name) / Path(path).parent
        dirname.mkdir(parents=True, exist_ok=True)
        (dirname / Path(path).name).write_text(content)


class ModusTestCase(unittest.TestCase):

    def __init__(self, methodName='runTest'):
        self._images = set()
        super().__init__(methodName)

    def _cleanup_images(self):
        for img in self._images:
            cmd = ["docker", "image", "rm", img]
            result = run(cmd, stdout=DEVNULL, stderr=DEVNULL)
        self._images.clear()

    def run(self, result=None):
        try:
            self.context = Context()
            super().run(result)
        finally:
            self.context.cleanup()
            self._cleanup_images()

    def build(self, modusfile, query, should_succeed=True):
        '''returns a mapping from facts to images'''
        with NamedTemporaryFile(mode="w+") as mf:
            mf.write(modusfile)
            mf.flush()
            with cd(self.context.name):
                cmd = [MODUS_EXECUTABLE, "build", self.context.name, "-f", mf.name, query, "--json"]
                if MODUS_BUILDKIT_FRONTEND:
                    cmd.extend(["--custom-buildkit-frontend", MODUS_BUILDKIT_FRONTEND])
                result = run(cmd, check=False, text=True, stdout=PIPE, stderr=PIPE)
                if should_succeed:
                    if result.returncode != 0:
                        raise Exception(f"Build failed:\n{result.stderr}\n\nModusfile:\n{modusfile}")
                    objects = json.load(StringIO(result.stdout))
                    images = { fact:img
                            for (fact, img)
                            in [(Fact(obj["predicate"], tuple([tuple(arg) if type(arg) == list else arg for arg in obj["args"]])), Image(obj["digest"]))
                                for obj
                                in objects] }
                    self._images.update([img.digest for img in images.values()])
                    return images
                else:
                    if result.returncode == 0:
                        raise Exception(f"Expected build to fail.")
                    return None
