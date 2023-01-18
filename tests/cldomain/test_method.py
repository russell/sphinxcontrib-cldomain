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


def test_method():
    result = sphinx_render_file("method")
    import pprint

    pprint.pp(result)
    assert result == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {
                        "desc_signature": "example-generic example-class "
                        ":test"
                    },
                    {
                        "desc_content": [
                            {
                                "desc": [
                                    {
                                        "desc_signature": "(setf "
                                        "(example-generic "
                                        "(arg1   "
                                        "EXAMPLE-CLASS)   "
                                        "(arg2   "
                                        "(eq   "
                                        ":TEST))) "
                                        "(new-value   "
                                        "COMMON-LISP:T))"
                                    }
                                ]
                            },
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "inline": "See "
                                        "also: "
                                        "example-generic"
                                    }
                                ]
                            },
                        ]
                    },
                ]
            },
        ]
    }


def test_method_noinherit():
    result = sphinx_render_file("method_noinherit")
    import pprint

    pprint.pp(result)
    assert sphinx_render_file("method_noinherit") == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {
                        "desc_signature": "example-generic example-class "
                        ":test"
                    },
                    {
                        "desc_content": [
                            {
                                "desc": [
                                    {
                                        "desc_signature": "(setf "
                                        "(example-generic "
                                        "(arg1   "
                                        "EXAMPLE-CLASS)   "
                                        "(arg2   "
                                        "(eq   "
                                        ":TEST))) "
                                        "(new-value   "
                                        "COMMON-LISP:T))"
                                    }
                                ]
                            },
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "inline": "See "
                                        "also: "
                                        "example-generic"
                                    }
                                ]
                            },
                        ]
                    },
                ]
            },
        ]
    }


def test_method_linkgeneric():
    result = sphinx_render_file("method_nolinkgeneric")
    import pprint

    pprint.pp(result)
    assert result == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {
                        "desc_signature": "example-generic example-class "
                        ":test"
                    },
                    {
                        "desc_content": [
                            {
                                "desc": [
                                    {
                                        "desc_signature": "(setf "
                                        "(example-generic "
                                        "(arg1   "
                                        "EXAMPLE-CLASS)   "
                                        "(arg2   "
                                        "(eq   "
                                        ":TEST))) "
                                        "(new-value   "
                                        "COMMON-LISP:T))"
                                    }
                                ]
                            },
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {"field_list": []},
                        ]
                    },
                ]
            },
        ]
    }


def test_method_nospecializers():
    result = sphinx_render_file("method_nospecializers")
    import pprint

    pprint.pp(result)
    assert result == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {
                        "desc_signature": "example-generic example-class "
                        ":test"
                    },
                    {
                        "desc_content": [
                            {
                                "desc": [
                                    {
                                        "desc_signature": "(setf "
                                        "(example-generic "
                                        "(arg1   "
                                        "EXAMPLE-CLASS)   "
                                        "(arg2   "
                                        "(eq   "
                                        ":TEST))) "
                                        "(new-value   "
                                        "COMMON-LISP:T))"
                                    }
                                ]
                            },
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "inline": "See "
                                        "also: "
                                        "example-generic"
                                    }
                                ]
                            },
                        ]
                    },
                ]
            },
        ]
    }
