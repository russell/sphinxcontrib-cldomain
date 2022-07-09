# -*- coding: utf-8 -*-
# cldomain is a Common Lisp domain for the Sphinx documentation tool.
# Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>

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

from sphinx.application import Sphinx


def setup(app: Sphinx) -> None:
    from .cldomain import (
        CLDomain,
        d_clparameter,
        d_clparameterlist,
        d_html_clparameter,
        d_latex_clparameter,
        d_latex_clparameterlist,
        desc_clparameter,
        desc_clparameterlist,
        list_unused_symbols,
        load_packages,
        v_clparameterlist,
        v_html_clparameter,
        v_latex_clparameter,
        v_latex_clparameterlist,
        v_texinfo_clparameter,
        v_text_clparameter,
    )

    app.add_domain(CLDomain)
    app.add_node(
        desc_clparameterlist,
        html=(v_clparameterlist, d_clparameterlist),
        latex=(v_latex_clparameterlist, d_latex_clparameterlist),
        texinfo=(v_clparameterlist, d_clparameterlist),
        text=(v_clparameterlist, d_clparameterlist),
    )
    app.add_node(
        desc_clparameter,
        html=(v_html_clparameter, d_html_clparameter),
        latex=(v_latex_clparameter, d_latex_clparameter),
        texinfo=(v_texinfo_clparameter, d_clparameter),
        text=(v_text_clparameter, d_clparameter),
    )
    app.add_config_value("cl_packages", {}, "env")
    app.add_config_value("cl_systems", [], "env")
    app.add_config_value("cl_show_defaults", False, True)
    app.add_config_value("cl_debug", False, "env")
    app.connect("builder-inited", load_packages)
    app.connect("build-finished", list_unused_symbols)
    # app.connect('source-read', uppercase_symbols)
