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
    assert sphinx_render_file("method") == {
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
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "field": [
                                            {"field_name": "Specializers"},
                                            {
                                                "field_body": [
                                                    {
                                                        "bullet_list": [
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class common-lisp:t)"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test2))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test1))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test))"
                                                                    }
                                                                ]
                                                            },
                                                        ]
                                                    }
                                                ]
                                            },
                                        ]
                                    }
                                ]
                            },
                            {"field_list": []},
                        ]
                    },
                ]
            },
        ],
    }


def test_method_noinherit():
    assert sphinx_render_file("method_noinherit") == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {"desc_signature": "example-generic example-class :test"},
                    {
                        "desc_content": [
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "field": [
                                            {"field_name": "Specializers"},
                                            {
                                                "field_body": [
                                                    {
                                                        "bullet_list": [
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class common-lisp:t)"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test2))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test1))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test))"
                                                                    }
                                                                ]
                                                            },
                                                        ]
                                                    }
                                                ]
                                            },
                                        ]
                                    }
                                ]
                            },
                            {"field_list": []},
                        ]
                    },
                ],
            },
        ],
    }


def test_method_linkgeneric():
    assert sphinx_render_file("method_linkgeneric") == {
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
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {
                                "field_list": [
                                    {
                                        "field": [
                                            {"field_name": "Specializers"},
                                            {
                                                "field_body": [
                                                    {
                                                        "bullet_list": [
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class common-lisp:t)"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test2))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test1))"
                                                                    }
                                                                ]
                                                            },
                                                            {
                                                                "list_item": [
                                                                    {
                                                                        "inline": "(example-class (eql :test))"
                                                                    }
                                                                ]
                                                            },
                                                        ]
                                                    }
                                                ]
                                            },
                                        ]
                                    }
                                ]
                            },
                            {
                                "field_list": [
                                    {
                                        "field": [
                                            {"field_name": "Generic"},
                                            {
                                                "field_body": [
                                                    {
                                                        "inline": "(example-class  :test )"
                                                    }
                                                ]
                                            },
                                        ]
                                    }
                                ]
                            },
                        ]
                    },
                ]
            },
        ],
    }


def test_method_nospecializers():
    result = sphinx_render_file("method_nospecializers")
    assert result == {
        "document": [
            {"index": []},
            {
                "desc": [
                    {"desc_signature": "example-generic example-class :test"},
                    {
                        "desc_content": [
                            {
                                "paragraph": "This is the first "
                                "specialized version "
                                "of example-generic."
                            },
                            {"field_list": []},
                        ]
                    },
                ],
            },
        ],
    }
