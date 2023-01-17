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

"""
    sphinxcontrib.cldomain
    ~~~~~~~~~~~~~~~~~~~~~~

    The Common Lisp domain

"""
import io
import json
import os
import pathlib
import pprint
import re
import subprocess
import sys
import tempfile
from os import path
from typing import List, Optional, Tuple, Union

from docutils import nodes
from docutils.nodes import Element, Node
from docutils.parsers.rst import Directive
from docutils.parsers.rst.states import Inliner
from docutils.statemachine import StringList, string2lines
from sphinx import addnodes
from sphinx.addnodes import desc_signature, pending_xref
from sphinx.application import Sphinx
from sphinx.builders import Builder
from sphinx.directives import ObjectDescription
from sphinx.domains import Domain, ObjType
from sphinx.environment import BuildEnvironment
from sphinx.locale import _
from sphinx.roles import XRefRole
from sphinx.util import logging
from sphinx.util.console import red
from sphinx.util.docfields import Field, GroupedField
from sphinx.util.nodes import make_refnode
from sphinx.util.typing import OptionSpec

__version__ = (
    open(path.join(path.dirname(__file__), "version.lisp-expr"))
    .read()
    .strip('"')
)


ALL_TYPES = [
    "macro",
    "function",
    "genericFunction",
    "setf",
    "variable",
    "class",
]
upper_symbols = re.compile(r"([^a-z\s\"`]*[A-Z]{2,}[^a-z\s\"`:]*)($|\s)")

LISP_DATA = {}

lambda_list_keywords = [
    "&allow-other-keys",
    "&key",
    "&rest",
    "&aux",
    "&optional",
]

logger = logging.getLogger(__name__)


def node_to_dict(node):
    name = getattr(node, "tagname", node)
    if getattr(node, "rawsource", None):
        return {name: node.rawsource}
    nodes = {name: []}
    for child in node.children:
        nodes[name].append(node_to_dict(child))
    return nodes


def debug_print(node):
    """Useful in pdb sessions."""
    node = node_to_dict(node)
    pprint.pprint(node)


def bool_option(arg: None) -> bool:
    """Used to convert flag options to directives.

    (Instead of directives.flag(), which returns None).
    """
    return True


# An almost exact copy of Peter Norvig's scheme parser
# http://norvig.com/lispy.html
def _read(s):
    """Read a Scheme expression from a string."""
    return _read_from(_tokenize(s))


def _tokenize(s: str) -> List[str]:
    """Convert a string into a list of tokens."""
    return s.replace("(", " ( ").replace(")", " ) ").split()


def _read_from(tokens):
    """Read an expression from a sequence of tokens."""
    if len(tokens) == 0:
        raise SyntaxError("unexpected EOF while reading")
    token = tokens.pop(0)
    if "(" == token:
        L = []
        while tokens[0] != ")":
            L.append(_read_from(tokens))
        tokens.pop(0)  # pop off ')'
        return L
    elif ")" == token:
        raise SyntaxError("unexpected )")
    else:
        return token


# end of http://norvig.com/lispy.html


class desc_clparameterlist(addnodes.desc_parameterlist):
    """Node for a common lisp parameter list."""

    child_text_separator = " "


# v is short for visit
# d is short for depart


def v_clparameterlist(self, node):
    self.first_param = True
    self.body.append(" ")
    self.body.append("(")
    self.param_separator = node.child_text_separator


def d_clparameterlist(self, node):
    self.body.append(")")


def v_latex_clparameterlist(self, node):
    if not hasattr(self, "parameter_stack"):
        self.parameter_stack = []

    if len(self.parameter_stack) == 0:
        # close name, open parameterlist
        self.body.append("}{")
    else:
        self.body.append(" (")

    self.parameter_stack.append(node)
    self.param_separator = node.child_text_separator


def d_latex_clparameterlist(self, node):
    self.parameter_stack.pop()
    if len(self.parameter_stack) == 0:
        # close parameterlist, open return annotation
        self.body.append("}{")
    else:
        self.body.append(" )")


class desc_clparameter(addnodes.desc_parameter):
    """Node for a common lisp parameter item."""


def d_clparameter(self, node):
    pass


def v_html_clparameter(self, node):
    if self.body[-1] != ("("):
        self.body.append(self.param_separator)
    if node.hasattr("lambda_keyword"):
        self.body.append('<em class="lambda_keyword text-muted">')
    elif node.hasattr("keyword"):
        self.body.append('<em class="keyword text-muted">')
    elif not node.hasattr("noemph"):
        self.body.append("<em>")


def d_html_clparameter(self, node):
    if node.hasattr("lambda_keyword"):
        self.body.append("</em>")
    elif node.hasattr("keyword"):
        self.body.append("</em>")
    elif not node.hasattr("noemph"):
        self.body.append("</em>")


def v_latex_clparameter(self, node):
    if self.body[-1] != ("("):
        self.body.append(self.param_separator)
    if node.hasattr("lambda_keyword") or node.hasattr("keyword"):
        pass
    elif not node.hasattr("noemph"):
        self.body.append(r"\emph{")


def d_latex_clparameter(self, node):
    if node.hasattr("lambda_keyword") or node.hasattr("keyword"):
        pass
    elif not node.hasattr("noemph"):
        self.body.append("}")


def v_texinfo_clparameter(self, node):
    if not self.first_param:
        self.body.append(self.param_separator)
    else:
        self.first_param = False
    text = self.escape(node.astext())
    # replace no-break spaces with normal ones
    text = text.replace(" ", "@w{ }")
    self.body.append(text)
    raise nodes.SkipNode


def v_text_clparameter(self, node):
    if not self.first_param:
        self.add_text(self.param_separator)
    else:
        self.first_param = False
    self.add_text(node.astext())
    raise nodes.SkipNode


def v_bs_html_desc_type(self, node):
    self.body.append(self.param_separator)
    self.body.append(self.starttag(node, "tt", "", CLASS="desc-type"))


def d_bs_html_desc_type(self, node):
    self.body.append("</tt>")


def v_html_desc_type(self, node):
    self.body.append(self.param_separator)


def specializer_qualify_symbols(
    symbols: List[str], package: Optional[str]
) -> List[str]:
    """Qualify symbols, for specializers."""

    def qualify(symbol):
        if isinstance(symbol, list):
            return [symbol[0].upper(), qualify(symbol[1])]
        # Depending on the splitting, it might be that EQ is a single
        # token, or split in 2.
        if symbol.startswith(":"):
            return ["EQ", "KEYWORD%s" % symbol.upper()]
        if symbol.lower() in lambda_list_keywords:
            return symbol.upper()

        # TODO (RS) this needs to be smarter what happens if there is an
        # internal symbol instead of an external one?
        if ":" not in symbol and package:
            return (package + ":" + symbol).upper()
        else:
            return symbol.upper()

    return [qualify(symbol) for symbol in symbols]


def parse_specializer_argument(argument: str, package: Optional[str]):
    """Convert the users arguments into a sexp list."""
    argument = argument.strip()
    # Always wrap in paren
    if not (argument.startswith("(") and argument.endswith(")")):
        argument = "(%s)" % argument
    return specializer_qualify_symbols(_read(argument), package)


def parse_specializer_lisp_data(argument: str, package: Optional[str]):
    """Process the specializer from LISP data to make sure it has the same
    formatting as the processed argument string."""
    argument = " ".join(argument).strip()
    return parse_specializer_argument(argument, package)


def normalize_specializer(specializer, package):
    """Convert the specializer into it's normal form as a string."""
    if isinstance(specializer, list):
        specializer = parse_specializer_lisp_data(specializer, package)
    else:
        specializer = parse_specializer_argument(specializer, package)
    return specializer_to_str(specializer)


def specializer_to_str(specializer):
    output = io.StringIO()

    def join_sexp(sexp):
        output.write("(")
        for i, atom in enumerate(sexp):
            if isinstance(atom, list):
                join_sexp(atom)
            else:
                output.write(atom)
            # Don't print a space after the last item.
            if i < len(sexp) - 1:
                output.write(" ")
        output.write(")")

    join_sexp(specializer)
    output.seek(0)
    return output.read()


def generic_xref(symbol: str, state, package: str, node_type=nodes.inline):
    xref = "See also: :cl:generic:`{} <{}:{}>`".format(
        symbol,
        package,
        symbol,
    )
    lines = string2lines(xref)
    node = node_type()
    state.nested_parse(StringList(lines), 0, node)
    return node


def specializer_name_xref(
    symbol: str, sexp, inliner: Inliner, package: str, lineno: int
):
    """Generate a link to a method.

    The output of this function is the partner to the output of
    CLMethod.get_index_name
    """
    target = normalize_specializer(sexp, package)
    target = "{} <{} {}>".format(
        local_atom(package, symbol.lower()),
        symbol.lower(),
        target.lower(),
    )
    xref = ":cl:method:`{}`".format(target)
    node = CLXRefRole()("cl:method", xref, target, lineno, inliner)
    return nodes.inline("", "", node[0][0])


def local_atom(package: str, atom: str, private: bool = True) -> str:
    """If the atom has a package qualifier then remove it."""
    split = [atom]
    if "::" in atom:
        if private:
            split = atom.split("::", 1)
        else:
            split = [atom]
    elif ":" in atom:
        split = atom.split(":", 1)

    if len(split) == 1:
        return split[0]

    if split[0].upper() == package.upper():
        return split[-1]

    # Remove the KEYWORD from those symbols
    if split[0].upper() == "KEYWORD":
        return ":" + split[-1]

    return atom


def qualify_atom(package: str, atom: str) -> str:
    """If the atom is not qualified then quailify it."""
    if "::" in atom:
        return atom
    elif ":" in atom:
        return atom
    return package + ":" + atom


def fieldlist_index(node):
    """Find the index of a field list in a content node."""
    for i, n in enumerate(node):
        if isinstance(n, nodes.field_list):
            return i


def get_content_node(node):
    """Search through and find the content node from a signature."""
    for subnode in node:
        if isinstance(subnode, addnodes.desc):
            for subsubnode in subnode:
                if isinstance(subsubnode, addnodes.desc_content):
                    return subsubnode


class LispDataError(Exception):
    pass


def get_lisp_object(package, name, objtype):
    symbol = ("%s:%s" % (package, name)).upper()
    objtype = objtype.strip()

    data = None
    try:
        if objtype in ["function", "macro", "generic", "method"]:
            data = LISP_DATA[symbol]["function"]
        else:
            data = LISP_DATA[symbol][objtype]
    except KeyError:
        raise LispDataError(
            "Missing lisp data for package %s, name %s, objtype %s"
            % (package, name, objtype)
        )
    if not data:
        raise LispDataError(
            "Missing lisp data for package %s, name %s, objtype %s"
            % (package, name, objtype)
        )
    return data


class SpecializerField(Field):
    """"""

    is_grouped = True
    list_type = nodes.bullet_list

    def __init__(
        self, name, names=(), label=None, rolename=None, can_collapse=False
    ):
        Field.__init__(self, name, names, label, True, rolename)
        self.can_collapse = can_collapse

    def make_field(self, domain, items):
        fieldname = nodes.field_name("", self.label)
        listnode = self.list_type()
        for content in items:
            par = nodes.paragraph()
            par += content
            listnode += nodes.list_item("", par)
        fieldbody = nodes.field_body("", listnode)
        return nodes.field("", fieldname, fieldbody)


class SEXP(object):
    """An SEXP argument list builder.

    This class takes an sexp and some types and will create a pretty
    argument list.
    """

    def __init__(self, sexp, types=None, show_defaults=False, package=None):
        if not isinstance(sexp, list):
            self.sexp = _read(sexp)
        else:
            self.sexp = sexp
        self.types = types
        if self.types:
            if len(self.sexp) < len(types):
                raise ValueError(
                    "Type expression %r, has more atoms than the sexp it's annotating %r"
                    % (types, sexp)
                )
            for i, type in enumerate(self.types):
                self.sexp[i] = [
                    self.sexp[i].lower(),
                    self._type_node(type, package),
                ]
        self.show_defaults = show_defaults
        self.show_defaults = True
        self.package = package

    def _type_node(self, type_name, package):
        if type_name.lower().startswith("(eq "):
            type_name = type_name.strip("()").split()[1]
            type_node = addnodes.pending_xref(
                "", refdomain="cl", reftype="class", reftarget=type_name
            )
            name = local_atom(package, type_name, private=False)
            type_node += addnodes.desc_type(name, name)
            return [addnodes.desc_sig_keyword("eq", "eq"), type_node]

        type_node = addnodes.pending_xref(
            "", refdomain="cl", reftype="class", reftarget=type_name
        )
        name = local_atom(package, type_name, private=False)
        type_node += addnodes.desc_type(name, name)
        return type_node

    def as_parameterlist(self, function_name):
        if isinstance(function_name, str) and function_name.startswith(
            "(SETF"
        ):
            # split up the "(setf foo)" string

            setf, name = function_name[1:-1].lower().split(" ")
            name = local_atom(self.package, name, private=False)
            name = addnodes.desc_name(name, name)
            desc_sexplist = desc_clparameterlist()
            if self.types:
                # this is a generic, so the first parameter becomes the value
                value = desc_clparameterlist()
                [self.render_atom(e, value) for e in self.sexp[0]]
                if value[-1].tagname == "desc_sig_space":
                    value.pop()  # if the last element is whitespace pop it
                self.sexp = self.sexp[1:]
            else:
                value = desc_clparameter("value", "value")
            desc_sexplist.append(addnodes.desc_sig_keyword(setf, setf))
            desc_sexplist.append(self.render_parameterlist(prepend_node=name))
            desc_sexplist.append(value)
            return desc_sexplist
        else:
            return self.render_parameterlist(prepend_node=function_name)

    def render_parameterlist(self, signode=None, prepend_node=None, sexp=None):
        desc_sexplist = desc_clparameterlist()
        if prepend_node:
            desc_sexplist.append(prepend_node)
        if signode:
            signode.append(desc_sexplist)
        for atom in sexp or self.sexp:
            if isinstance(atom, list):
                if self.show_defaults:
                    self.render_parameterlist(signode=desc_sexplist, sexp=atom)
                    desc_sexplist.append(addnodes.desc_sig_space())
                else:
                    self.render_atom(atom[0], desc_sexplist)
            else:
                self.render_atom(atom, desc_sexplist)
        if desc_sexplist[-1].tagname == "desc_sig_space":
            desc_sexplist.pop()  # if the last element is whitespace pop it
        return desc_sexplist

    def render_atom(self, token, signode, noemph=True):
        """add syntax hi-lighting to interesting atoms."""
        if not isinstance(token, nodes.Element):
            param = desc_clparameter(token, local_atom(self.package, token))
            if token.lower() in lambda_list_keywords:
                param = addnodes.desc_sig_keyword(
                    token, local_atom(self.package, token)
                )
                param["lambda_keyword"] = True
            if token.startswith(":"):
                param["keyword"] = True
            signode.append(param)
            signode.append(addnodes.desc_sig_space())
        else:
            signode.append(token)
            signode.append(addnodes.desc_sig_space())


class CLsExp(ObjectDescription):

    doc_field_types = [
        GroupedField(
            "parameter",
            label=_("Parameters"),
            names=(
                "param",
                "parameter",
                "arg",
                "argument",
                "keyword",
                "kwparam",
            ),
        ),
        Field(
            "returnvalue",
            label=_("Returns"),
            has_arg=False,
            names=("returns", "return"),
        ),
    ]

    option_spec: OptionSpec = {
        "nodoc": bool_option,
        "noindex": bool_option,
        "noinitargs": bool_option,
    }

    lispobj = False

    def cl_lisp_data(self):
        """Initialize the self.lispobj object.

        This is done using data that is passed to the application from a
        CL lisp process.
        """
        if self.lispobj is not False:
            return self.lispobj

        self.lispobj = None
        try:
            self.lispobj = get_lisp_object(
                self.cl_package, self.cl_symbol_name, self.objtype
            )
        except LispDataError as e:
            self.state_machine.reporter.warning(e)

        return self.lispobj

    def cl_handle_signature(self, sig: str, signode: desc_signature):
        pass

    def handle_signature(
        self, sig: str, signode: desc_signature
    ) -> Tuple[str, str]:
        """This method sets up the entire object, it's the first."""
        self.cl_package = self.env.temp_data.get("cl:package").lower()
        self.cl_symbol_name = sig.split(" ")[0].lower()

        signode.append(
            addnodes.desc_annotation(
                str(self.get_signature_prefix()), self.get_signature_prefix()
            )
        )

        self.cl_handle_signature(sig, signode)

        if not self.cl_symbol_name:
            self.state_machine.reporter.warning(
                "Unknown symbol type for signature %s" % self.cl_symbol_name
            )

        return self.objtype, self.cl_symbol_name

    def get_field_list(self, node):
        """Return the node's field list, if there isn't one then create it
        first."""
        # Add a field list if there isn't one
        if not node[1][-1].children:
            node[1][-1].append(nodes.field_list())
        if not isinstance(node[1][-1][0], nodes.field_list):
            node[1][-1].append(nodes.field_list())
        return node[1][-1][-1]

    def get_index_text(self, name, type):
        return _("%s (Lisp %s)") % (name.lower().split(":")[-1], type)

    def get_index_name(self, name, type):
        return type + ":" + name

    def get_signature_prefix(self):
        return self.objtype + " "

    def add_target_and_index(self, name, sig, signode):
        # node target
        type, name = name

        if "cl:package" in self.env.temp_data:
            package = self.options.get(
                "module", self.env.temp_data.get("cl:package")
            )
            name = package.lower() + ":" + name
        else:
            return

        indexname = self.get_index_name(name, type)
        if name not in self.state.document.ids:
            signode["names"].append(name)
            signode["ids"].append(indexname)
            signode["first"] = not self.names
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata["cl"]["symbols"]
            # TODO (RS) reenable this checking based on doc and type.
            # if name in inv:
            #     self.state_machine.reporter.warning(
            #         'duplicate symbol description of %s, ' % name +
            #         'other instance in ' + self.env.doc2path(inv[name][0]),
            #         line=self.lineno)
            if name in inv:
                inv[name].append((self.env.docname, self.objtype))
            else:
                inv[name] = [(self.env.docname, self.objtype)]

        indextext = self.get_index_text(name, type)
        if indextext:
            self.indexnode["entries"].append(
                ("single", indextext, indexname, "", None)
            )

    def before_content(self):
        if "nodoc" in self.options:
            return
        package = self.env.temp_data.get("cl:package")
        name = self.names[0][1]
        if not package:
            self.state_machine.reporter.warning(
                "No package specified for symbol %s." % name
            )
            return
        string = self.cl_doc_string()
        if not string:
            return
        lines = string2lines(string) + [""]
        self.content = StringList(lines) + self.content

    def cl_doc_string(self, objtype=None):
        """Resolve a symbols doc string.

        Will raise KeyError if the symbol can't be found.
        """
        objtype = objtype or self.objtype
        lispobj = get_lisp_object(
            self.cl_package, self.cl_symbol_name, objtype
        )
        if "documentation" in lispobj:
            return lispobj["documentation"]

        self.state_machine.reporter.warning(
            "Can't find symbol {}:{}".format(
                self.cl_package, self.cl_symbol_name
            )
        )
        return ""


class CLVariable(CLsExp):
    def cl_handle_signature(self, sig: str, signode: desc_signature):
        """Perform CLOS specific signature additions."""
        name = addnodes.desc_name(self.cl_symbol_name, self.cl_symbol_name)
        signode.append(name)


class CLClass(CLsExp):
    def cl_handle_signature(self, sig: str, signode: desc_signature):
        """Perform CLOS specific signature additions."""
        name = addnodes.desc_name(self.cl_symbol_name, self.cl_symbol_name)
        signode.append(name)

        # Add CLOS Slots
        lispobj = self.cl_lisp_data()
        slots = lispobj["slots"]
        if slots and "noinitargs" not in self.options:
            # TODO add slot details if describing a class
            for slot in slots:
                initarg = slot.get("initarg")
                if initarg and initarg.lower() != "nil":
                    slotarg = addnodes.literal_emphasis(
                        slot.get("name"), slot.get("name")
                    )
                    slotsig = initarg.lower() + " "
                    signode.append(
                        addnodes.desc_optional(slotsig, slotsig, slotarg)
                    )


class CLFunction(CLsExp):
    def get_signature_prefix(self):
        return self.objtype

    def cl_handle_signature(self, sig: str, signode: desc_signature):
        """Perform CLOS specific signature additions."""
        name = addnodes.desc_name(self.cl_symbol_name, self.cl_symbol_name)

        lispobj = self.cl_lisp_data()
        lisp_args = lispobj["arguments"].strip() or "()"
        try:
            sexp = SEXP(
                lisp_args.lower(),
                show_defaults=self.env.app.config.cl_show_defaults,
                package=self.env.temp_data.get("cl:package"),
            )
            arg_list = sexp.as_parameterlist(name)
            signode.append(arg_list)
        except ValueError as e:
            self.state_machine.reporter.warning(e)

    def transform_content(self, contentnode: addnodes.desc_content) -> None:
        dl = addnodes.desc()
        dl["objtype"] = "setfFunction"
        dd = addnodes.desc_content()
        dd.append(dl)
        contentnode.children.insert(0, dl)

        self.cl_handle_setf(dl)

    def cl_handle_setf(self, contentnode):
        try:
            setfobj = get_lisp_object(
                self.cl_package, self.cl_symbol_name, "setfFunction"
            )
        except LispDataError:
            return

        if setfobj["type"] == "genericFunction":
            self.cl_handle_setf_methods(contentnode, setfobj["methods"])
        else:
            self.cl_handle_setf_function(contentnode, setfobj)

    def cl_handle_setf_function(self, methodnode, setfobj):
        signode = addnodes.desc_signature("", "")
        sexp = SEXP(
            setfobj["arguments"].lower(),
            show_defaults=self.env.app.config.cl_show_defaults,
            package=self.cl_package,
        )
        arg_list = sexp.as_parameterlist(setfobj["name"])
        signode.append(arg_list)
        methodnode.append(signode)

    def cl_handle_setf_methods(self, methodnode, methodsobj):
        for method in methodsobj:
            signode = addnodes.desc_signature("", "")
            sexp = SEXP(
                method["arguments"].lower(),
                types=method["specializer"],
                show_defaults=self.env.app.config.cl_show_defaults,
                package=self.cl_package,
            )
            arg_list = sexp.as_parameterlist(method["name"])
            signode.append(arg_list)
            methodnode.append(signode)


class CLMacro(CLFunction):
    pass


class CLGeneric(CLFunction):

    option_spec: OptionSpec = {
        "nodoc": bool_option,
        "noindex": bool_option,
        "nospecializers": bool_option,
    }

    def transform_content(self, contentnode: addnodes.desc_content) -> None:
        if self.objtype != "generic":
            return
        if "nospecializers" in self.options:
            return
        package = self.env.temp_data.get("cl:package")
        name = self.cl_symbol_name
        lispobj = get_lisp_object(package, name, self.objtype)

        dl = addnodes.desc()
        # This objtype setting is needed to support the latex output.
        dl["objtype"] = "method"
        dd = addnodes.desc_content()
        dd.append(dl)
        contentnode.children.insert(0, dl)
        for method in lispobj["methods"]:
            signode = addnodes.desc_signature("", "")
            # signode.append(addnodes.desc_annotation("method", "method"))
            name = local_atom(package, method["name"]).lower()
            ref = specializer_name_xref(
                method["name"],
                method["specializer"],
                self.state.inliner,
                package,
                self.lineno,
            )
            function_name = addnodes.desc_name()
            function_name.append(ref)
            sexp = SEXP(
                method["arguments"],
                types=method["specializer"],
                show_defaults=self.env.app.config.cl_show_defaults,
                package=self.env.temp_data.get("cl:package"),
            )
            arg_list = sexp.as_parameterlist(function_name)
            signode.append(arg_list)
            dl.append(signode)
        super(CLGeneric, self).cl_handle_setf(dl)


class CLMethod(CLFunction):

    option_spec: OptionSpec = {
        "nodoc": bool_option,
        "noindex": bool_option,
        "noinherit": bool_option,
        "nospecializers": bool_option,
        "nolinkgeneric": bool_option,
    }

    doc_field_types = [
        Field(
            "specializer",
            label=_("Specializer"),
            has_arg=False,
            names=("specializer"),
        ),
        GroupedField(
            "parameter",
            label=_("Parameters"),
            names=(
                "param",
                "parameter",
                "arg",
                "argument",
                "keyword",
                "kwparam",
            ),
        ),
        Field(
            "returnvalue",
            label=_("Returns"),
            has_arg=False,
            names=("returns", "return"),
        ),
    ]

    def cl_method_specializer(self):
        """"""
        return normalize_specializer(
            self.arguments[0].split(" ")[1:], self.cl_package
        )

    def cl_method(self, sig: str, package: str):
        """Convert find a method using the sig and package.

        sig would be e.g. `my-method (eq foobar) my-class`
        """
        symbol_name = sig.split(" ")[0]  # get the name of the method
        lispobj = get_lisp_object(package, symbol_name, "method")

        # remove the function name from the specializer arguments
        key = self.cl_method_specializer()

        try:
            return next(
                m
                for m in lispobj["methods"]
                if normalize_specializer(m["specializer"], self.cl_package)
                == key
            )
        except StopIteration:
            specializers = [
                normalize_specializer(m["specializer"], self.cl_package)
                for m in lispobj["methods"]
            ]
            self.state_machine.reporter.warning(
                "Can't find method %s:%s specializer %s, "
                "available specializers are %s"
                % (package, symbol_name, key, specializers)
            )

    def cl_setf_methods(self, symbol_name: str, package: str):
        """Convert find a method using the sig and package.

        sig would be e.g. `my-method (eq foobar) my-class`
        """
        lispobj = get_lisp_object(package, symbol_name, "setfFunction")

        # remove the function name and the first type from the
        # specializer arguments
        key = self.cl_method_specializer()

        try:
            return [
                m
                for m in lispobj["methods"]
                # Remove the first type from the specializer
                if normalize_specializer(m["specializer"][1:], self.cl_package)
                == key
            ]
        except StopIteration:
            specializers = [
                normalize_specializer(m["specializer"], self.cl_package)
                for m in lispobj["methods"]
            ]
            self.state_machine.reporter.warning(
                "Can't find method %s:%s specializer %s, "
                "available specializers are %s"
                % (package, symbol_name, key, specializers)
            )

    def cl_handle_setf(self, contentnode):
        try:
            setfobj = get_lisp_object(
                self.cl_package, self.cl_symbol_name, "setfFunction"
            )
        except LispDataError:
            return

        if setfobj["type"] == "genericFunction":
            self.cl_handle_setf_methods(
                contentnode,
                self.cl_setf_methods(self.cl_symbol_name, self.cl_package),
            )
        else:
            self.cl_handle_setf_function(contentnode, setfobj)

    def cl_handle_signature(self, sig: str, signode: desc_signature):
        """Perform Method specific signature additions."""
        name = addnodes.desc_name(self.cl_symbol_name, self.cl_symbol_name)

        lispobj = self.cl_lisp_data()
        lisp_args = lispobj["arguments"].strip() or "()"
        # returns none if not found
        types = self.cl_method(sig, self.cl_package)
        if types:
            types = types["specializer"]
        try:
            sexp = SEXP(
                lisp_args.lower(),
                types=types,
                show_defaults=self.env.app.config.cl_show_defaults,
                package=self.env.temp_data.get("cl:package"),
            )
            arg_list = sexp.as_parameterlist(name)
            signode.append(arg_list)
        except ValueError as e:
            self.state_machine.reporter.warning(e)

    def get_index_name(self, name, type):
        """Generate a name that will be used to create anchors on the page for
        each method."""
        return "%s:%s %s" % (
            type,
            qualify_atom(self.cl_package, self.cl_symbol_name).lower(),
            self.cl_method_specializer().lower(),
        )

    def get_index_text(self, name, type):
        specializer = self.cl_method_specializer()
        return _("%s (%s) (Lisp %s)") % (
            name.lower().split(":")[-1],
            specializer.lower(),
            type,
        )

    def add_target_and_index(
        self, name_obj: Tuple[str, str], sig: str, signode: desc_signature
    ) -> None:
        # node target
        type, name = name_obj

        if "cl:package" in self.env.temp_data:
            package = self.options.get(
                "module", self.env.temp_data.get("cl:package")
            )
            name = package.lower() + ":" + name
        else:
            return

        indexname = self.get_index_name(name, type)
        if name not in self.state.document.ids:
            signode["names"].append(name)
            signode["ids"].append(indexname)
            signode["first"] = not self.names
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata["cl"]["methods"]
            # TODO (RS) reenable this checking based on doc and type.
            # if name in inv:
            #     self.state_machine.reporter.warning(
            #         'duplicate symbol description of %s, ' % name +
            #         'other instance in ' + self.env.doc2path(inv[name][0]),
            #         line=self.lineno)

            sig = self.cl_method_specializer()
            if name in inv:
                inv[name][sig] = (self.env.docname, self.objtype)
            else:
                inv[name] = {sig: (self.env.docname, self.objtype)}

        indextext = self.get_index_text(name, type)
        if indextext:
            self.indexnode["entries"].append(
                ("single", indextext, indexname, "", None)
            )

    def cl_doc_string(self):
        """Resolve a symbols doc string.

        Will raise KeyError if the symbol can't be found.
        """
        doc = ""
        method = self.cl_method(self.arguments[0], self.cl_package)
        if method:
            doc = method["documentation"]

        if doc:
            return doc

        if "noinherit" not in self.options:
            return super(CLMethod, self).cl_doc_string("generic")
        return ""

    def run(self) -> List[Node]:
        result = super(CLMethod, self).run()
        field_list = self.get_field_list(result)
        package = self.env.temp_data.get("cl:package")

        if "nolinkgeneric" not in self.options:
            spec = generic_xref(
                self.cl_symbol_name,
                self.state,
                package=package,
            )
            field_list.append(spec)

        return result


class CLCurrentPackage(Directive):
    """This directive is just to tell Sphinx that we're documenting stuff in
    namespace foo."""

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec: OptionSpec = {}

    def run(self):
        env = self.state.document.settings.env
        env.temp_data["cl:package"] = self.arguments[0].upper()
        # index_package(self.arguments[0].upper())
        return []


class CLXRefRole(XRefRole):
    def process_link(
        self,
        env: BuildEnvironment,
        refnode: Element,
        has_explicit_title: bool,
        title: str,
        target: str,
    ) -> Tuple[str, str]:
        if not has_explicit_title:
            target = target.lstrip("~")  # only has a meaning for the title
            # if the first character is a tilde, don't display the package
            if title[0:1] == "~":
                symbol = title[1:].split(":")
                # package = symbol[0]
                title = symbol[-1]
                if target[0] == ":":
                    title = ":" + title
        return title, target


class CLDomain(Domain):
    """CL language domain."""

    name = "cl"
    label = "Common Lisp"

    object_types = {
        "package": ObjType(_("package"), "package"),
        "function": ObjType(_("function"), "function"),
        "macro": ObjType(_("macro"), "macro"),
        "variable": ObjType(_("variable"), "variable"),
        "class": ObjType(_("class"), "class"),
        "generic": ObjType(_("generic"), "generic"),
        "method": ObjType(_("method"), "method"),
    }

    directives = {
        "package": CLCurrentPackage,
        "function": CLFunction,
        "generic": CLGeneric,
        "macro": CLMacro,
        "variable": CLVariable,
        "class": CLClass,
        "method": CLMethod,
    }

    roles = {
        "symbol": CLXRefRole(),
        "function": CLXRefRole(),
        "generic": CLXRefRole(),
        "macro": CLXRefRole(),
        "variable": CLXRefRole(),
        "class": CLXRefRole(),
        "method": CLXRefRole(),
    }
    initial_data = {
        "symbols": {},
        "methods": {},
    }

    def clear_doc(self, docname: str) -> None:
        for fullname, docs in self.data["symbols"].copy().items():
            for (fn, _ignored) in docs:
                if fn == docname:
                    del self.data["symbols"][fullname]
        return None

    def find_obj(
        self, env: BuildEnvironment, name: str
    ) -> Optional[Union[List[Tuple[str, List[Tuple[str, str]]]], filter]]:
        """Find a Lisp symbol for "name", perhaps using the given package
        Return a list of (name, object entry) tuples."""
        symbols = self.data["symbols"]
        name = name.lower()
        if ":" in name:
            if name in symbols:
                return [(name, symbols[name])]
        else:

            def filter_symbols(symbol):
                symbol = symbol[0]
                if name == symbol:
                    return True
                if ":" in symbol:
                    symbol = symbol.split(":")[1]
                    if name == symbol:
                        return True
                return False

            return filter(filter_symbols, symbols.items())

    def find_method(
        self, env: BuildEnvironment, name: str, node: pending_xref
    ) -> Optional[List[Tuple[str, str]]]:
        """Find a Lisp symbol for "name", perhaps using the given package
        Return a list of (name, object entry) tuples."""
        methods = self.data["methods"]
        name = name.lower()
        sexp = name.split(" ")
        generic = sexp[0]
        specializer = " ".join(sexp[1:]).upper()

        if generic in methods:
            if specializer in methods[generic]:
                return [methods[generic][specializer]]
            else:
                logger.warning("can't find method %s" % (name), location=node)
        else:
            logger.warning("can't find generic %s" % (name), location=node)

    def resolve_xref(
        self,
        env: BuildEnvironment,
        fromdocname: str,
        builder: Builder,
        typ: str,
        target: str,
        node: pending_xref,
        contnode: Element,
    ) -> Optional[Element]:
        if " " in target:
            matches = self.find_method(env, target.upper(), node)
        else:
            matches = self.find_obj(env, target.upper())

        if not matches:
            return None

        matches = [*matches]
        if len(matches) == 0:
            logger.warning(
                "no target found for cross-reference %r" % (target),
                location=node,
            )
            return None
        elif len(matches) > 1:
            logger.warning(
                "more than one target found for cross-reference "
                "%r: %s" % (target, ", ".join(match[0] for match in matches)),
                location=node,
            )
        # TODO (RS) this just chooses the first symbol, instead every
        # symbol should be presented.

        if " " in target:
            sexp = target.split(" ")
            generic = sexp[0].lower()
            specializer = " ".join(sexp).lower()
            name = generic
            filename = matches[0][0]  # the first filename
            link = "method" + ":" + specializer
        else:
            name = matches[0][0]  # the symbol name
            filename = matches[0][1][0][0]  # the first filename
            type = matches[0][1][0][1]  # the first type
            link = type + ":" + name
        return make_refnode(
            builder, fromdocname, filename, link, contnode, name
        )

    def get_symbols(self):
        for refname, docs in self.data["symbols"].items():
            for (docname, type) in docs:
                yield (refname, refname, type, docname, refname, 1)


def save_cldomain_output(output: bytes):
    """Save a copy of the clgit output for debugging."""
    fd, path = tempfile.mkstemp(".log", "cldomain-err-")
    os.write(fd, output)
    os.close(fd)
    return path


def index_packages(
    systems: List[str],
    system_paths: List[str],
    packages: List[str],
    cl_debug: bool,
) -> None:
    """Call an external lisp program that will return a dictionary of doc
    strings for all public symbols."""
    cldomain_exe = [
        str(pathlib.Path(__file__).parent.resolve().joinpath("cldomain.ros"))
    ]
    cldomain_args = [
        "--package",
        ",".join(packages),
        "--system",
        ",".join(systems),
        "--path",
        ",".join(system_paths),
    ]
    env = os.environ.copy()
    raw_output = subprocess.check_output(cldomain_exe + cldomain_args, env=env)
    try:
        lisp_data = json.loads(raw_output)
        if cl_debug:
            pprint.pprint(lisp_data)
    except Exception:
        dump_path = save_cldomain_output(raw_output)
        print(
            red(
                "A error occurred with the json output from cldomain's"
                " lisp inspector,  this has been dumped to %s if you "
                "intend on submitting a bug please include this file "
                "with the sphinx error log." % dump_path
            ),
            file=sys.stderr,
        )
        raise
    LISP_DATA.update(lisp_data)


def load_packages(app: Sphinx) -> None:
    packages = []
    systems = []
    system_paths = []
    if app.config.cl_packages:
        logger.info(
            "DEPRECATED: The cl_packages variable has been "
            "replaced by cl_systems and will be removed in the future."
        )
        for package, system_path in app.config.cl_packages.items():
            packages.append(package.upper())
            systems.append(package)
            system_paths.append(system_path)
    if app.config.cl_systems:
        for system in app.config.cl_systems:
            systems.append(system["name"])

            if "path" in system:
                system_paths.append(system["path"])

            if "packages" in system:
                for package in system["packages"]:
                    packages.append(package.upper())
            else:
                packages.append(system["name"].upper())

    if not packages:
        logger.warn("No CL packages specified.")
        return None

    logger.info(
        "Collecting Lisp docstrings from %s..."
        % ", ".join(str(x) for x in systems)
    )
    index_packages(
        systems,
        system_paths,
        packages,
        app.config.cl_debug,
    )


def uppercase_symbols(app, docname, source):
    """For each line in a list replace all uppercase symbols with a sphinx
    references."""
    for i, line in enumerate(source):
        source[i] = re.sub(upper_symbols, r":cl:symbol:`~\g<1>`\g<2>", line)


def add_node(class_name, node, visit, depart=None):
    """Register a node's visitor functions with a class, if is available."""

    def import_class(cl):
        d = cl.rfind(".")
        classname = cl[d + 1 : len(cl)]
        m = __import__(cl[0:d], globals(), locals(), [classname])
        return getattr(m, classname)

    try:
        translator = import_class(class_name)
    except (ImportError, AttributeError):
        return None
    setattr(translator, "visit_" + node.__name__, visit)
    if depart:
        setattr(translator, "depart_" + node.__name__, depart)


add_node(
    "sphinx_bootstrap_theme.BootstrapTranslator",
    desc_clparameterlist,
    v_clparameterlist,
    d_clparameterlist,
)

add_node(
    "sphinx_bootstrap_theme.BootstrapTranslator",
    desc_clparameter,
    v_html_clparameter,
    d_html_clparameter,
)

add_node(
    "sphinx_bootstrap_theme.BootstrapTranslator",
    addnodes.desc_type,
    v_bs_html_desc_type,
    d_bs_html_desc_type,
)

add_node(
    "sphinx.writers.html.HTMLTranslator", addnodes.desc_type, v_html_desc_type
)
