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

from ..support.nodes import sphinx_render_file


def test_function():
    result = sphinx_render_file("function")
    import pprint

    pprint.pp(result)
    assert result == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {"desc_signature": "example-function"},
                    {
                        "desc_content": [
                            {
                                "desc": [
                                    {
                                        "desc_signature": "(setf "
                                        "(example-function "
                                        "arg1   "
                                        "arg2) "
                                        "value)"
                                    }
                                ]
                            },
                            {
                                "paragraph": "Example function CL "
                                "Domain\n"
                                "Reference "
                                ":cl:symbol:`~SPHINXCONTRIB.CLDOMAIN.DOC-TEST:EXAMPLE-FUNCTION`\n"
                                "Package references "
                                ":cl:symbol:`~COMMON-LISP:CDR`\n"
                                "Hyperspec "
                                ":cl:symbol:`~COMMON-LISP:LIST`\n"
                                "Arguments ``ARG1``, "
                                "``ARG2`` and "
                                "``ARG3``\n"
                                "Keyword "
                                "``:TEST``"
                            },
                        ]
                    },
                ]
            },
        ]
    }
