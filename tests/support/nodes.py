# -*- coding: utf-8 -*-
# cldomain is a Common Lisp domain for the Sphinx documentation tool.
# Copyright (C) 2022 Russell Sim <russell.sim@gmail.com>

# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import sys
from os import path

from sphinx.application import Sphinx

from .null_builder import setup as null_builder_setup

FIXTURES_DIR = path.join(path.dirname(__file__), "..", "fixtures")


def sphinx_render_file(filename):
    app = Sphinx(
        FIXTURES_DIR,
        FIXTURES_DIR,
        path.join(FIXTURES_DIR, "output"),
        path.join(FIXTURES_DIR, "output", ".doctrees"),
        "dummy",
        confoverrides={
            "master_doc": filename,
        },
        status=sys.stdout,
        warning=sys.stderr,
        freshenv=True,
        warningiserror=False,
        tags=[],
        verbosity=0,
        parallel=1,
        keep_going=False,
    )
    null_builder_setup(app)
    app.builder = app.create_builder("null")
    app._init_builder()
    app.build(False, [filename])
    return node_to_dict(app.builder.root_nodes[-1])


def node_to_dict(node):
    name = getattr(node, "tagname", node)
    if getattr(node, "rawsource", None):
        return {name: node.rawsource}
    nodes = {name: []}
    for child in node.children:
        nodes[name].append(node_to_dict(child))
    return nodes
