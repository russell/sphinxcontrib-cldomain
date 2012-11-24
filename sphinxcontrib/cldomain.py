# -*- coding: utf-8 -*-
# cldomain is a Common Lisp domain for the Sphinx documentation tool.
# Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>

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
    sphinx.domains.cl
    ~~~~~~~~~~~~~~~~~

    The Common Lisp domain


    TODO
    ----
    add source-read handler
    http://sphinx.pocoo.org/latest/ext/appapi.html#event-source-read
"""
import re
from os import path
import json
import subprocess
from docutils import nodes
from docutils.statemachine import string2lines, StringList

from sphinx import addnodes
from sphinx.locale import l_, _
from sphinx.roles import XRefRole
from sphinx.domains import Domain, ObjType
from sphinx.directives import ObjectDescription
from sphinx.util.nodes import make_refnode
from sphinx.util.compat import Directive
from sphinx.util.docfields import Field, GroupedField
from sphinx.util.docfields import DocFieldTransformer

TYPES = ["macro", "function", "genericFunction", "setf", "variable", "type"]
upper_symbols = re.compile("(^|\s)([^a-z\s\"`]*[A-Z]{2,}[^a-z\s\"`:]*)($|\s)")

doc_strings = {}
types = {}
args = {}
specializers = {}


def bool_option(arg):
    """Used to convert flag options to directives.  (Instead of
    directives.flag(), which returns None).
    """
    return True


# An almost exact copy of Peter Norvig's scheme parser
# http://norvig.com/lispy.html
def _read(s):
    "Read a Scheme expression from a string."
    return _read_from(_tokenize(s))


def _tokenize(s):
    "Convert a string into a list of tokens."
    return s.replace('(', ' ( ').replace(')', ' ) ').split()


def _read_from(tokens):
    "Read an expression from a sequence of tokens."
    if len(tokens) == 0:
        raise SyntaxError('unexpected EOF while reading')
    token = tokens.pop(0)
    if '(' == token:
        L = []
        while tokens[0] != ')':
            L.append(_read_from(tokens))
        tokens.pop(0)  # pop off ')'
        return L
    elif ')' == token:
        raise SyntaxError('unexpected )')
    else:
        return token
# end of http://norvig.com/lispy.html


def resolve_string(package, symbol, objtype):
    possible_strings = doc_strings.get(package).get(symbol, {})
    string = possible_strings.get(objtype, "")
    return string


class CLsExp(ObjectDescription):

    doc_field_types = [
        GroupedField('parameter', label=l_('Parameters'),
                     names=('param', 'parameter', 'arg', 'argument',
                            'keyword', 'kwparam')),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
    ]

    option_spec = {
        'nodoc': bool_option, 'noindex': bool_option,
    }

    def handle_signature(self, sig, signode):
        symbol_name = []
        type = []

        def render_sexp(sexp, signode=None, prepend_node=None):
            desc_sexplist = addnodes.desc_parameterlist()
            desc_sexplist.child_text_separator = ' '
            if prepend_node:
                desc_sexplist.append(prepend_node)
            if signode:
                signode.append(desc_sexplist)
            symbol = False
            for atom in sexp:
                if isinstance(atom, list):
                    render_sexp(atom, desc_sexplist)
                else:
                    symbol = render_atom(atom, desc_sexplist)
            return desc_sexplist

        def render_atom(token, signode, noemph=True):
            "add syntax hi-lighting to interesting atoms"

            if token.startswith("&") or token.startswith(":"):
                signode.append(addnodes.desc_parameter(token, token))
            else:
                signode.append(addnodes.desc_parameter(token, token))

        package = self.env.temp_data.get('cl:package')

        objtype = self.get_signature_prefix(sig)
        signode.append(addnodes.desc_annotation(objtype, objtype))
        lisp_args = args[package].get(sig.upper(), "")

        if lisp_args.strip():
            function_name = addnodes.desc_name(sig, sig + " ")
        else:
            function_name = addnodes.desc_name(sig, sig)

        if not lisp_args.strip() and self.objtype in ["function"]:
            lisp_args = "()"
        if lisp_args.strip():
            arg_list = render_sexp(_read(lisp_args),
                                   prepend_node=function_name)
            signode.append(arg_list)
        else:
            signode.append(function_name)

        symbol_name = sig
        if not symbol_name:
            raise Exception("Unknown symbol type for signature %s" % sig)
        return objtype.strip(), symbol_name.upper()

    def get_index_text(self, name, type):
        return _('%s (Lisp %s)') % (name, type)

    def get_signature_prefix(self, sig):
        return self.objtype + ' '

    def add_target_and_index(self, name, sig, signode):
        # note target
        type, name = name

        if 'cl:package' in self.env.temp_data:
            package = self.options.get(
                'module', self.env.temp_data.get('cl:package'))
            name = package + ":" + name
        else:
            package = ""

        if name not in self.state.document.ids:
            signode['names'].append(name)
            signode['ids'].append(name)
            signode['first'] = (not self.names)
            self.state.document.note_explicit_target(signode)
            inv = self.env.domaindata['cl']['symbols']
            if name in inv:
                self.state_machine.reporter.warning(
                    'duplicate symbol description of %s, ' % name +
                    'other instance in ' + self.env.doc2path(inv[name][0]),
                    line=self.lineno)
            inv[name] = (self.env.docname, self.objtype)

        indextext = self.get_index_text(name, type)
        if indextext:
            self.indexnode['entries'].append(('single', indextext, name, ''))

    def run(self):
        result = super(CLsExp, self).run()
        if "nodoc" not in self.options:
            package = self.env.temp_data.get('cl:package')
            node = addnodes.desc_content()
            string = resolve_string(package, self.names[0][1], self.objtype)
            import pdb; pdb.set_trace()
            lines = string2lines(string)
            self.state.nested_parse(StringList(lines), 0, node)
            if (result[1][1].children and
                isinstance(result[1][1][0], nodes.field_list)):
                cresult = result[1][1].deepcopy()
                target = result[1][1]
                target.clear()
                target.append(cresult[0])
                target.extend(node)
                target.extend(cresult[1:])
            else:
                cresult = result[1][1].deepcopy()
                target = result[1][1]
                target.clear()
                target.extend(node)
                target.extend(cresult)
        return result


class CLCurrentPackage(Directive):
    """
    This directive is just to tell Sphinx that we're documenting stuff in
    namespace foo.
    """

    has_content = False
    required_arguments = 1
    optional_arguments = 0
    final_argument_whitespace = True
    option_spec = {}

    def run(self):
        env = self.state.document.settings.env
        env.temp_data['cl:package'] = self.arguments[0].upper()
        #index_package(self.arguments[0].upper())
        return []


class CLXRefRole(XRefRole):
    def process_link(self, env, refnode, has_explicit_title, title, target):
        if not has_explicit_title:
            target = target.lstrip('~')  # only has a meaning for the title
            # if the first character is a tilde, don't display the package
            if title[0:1] == '~':
                title = title[1:]
                dot = title.rfind(':')
                if dot != -1:
                    title = title[dot + 1:]
        return title, target


class CLDomain(Domain):
    """CL language domain."""
    name = 'cl'
    label = 'Common Lisp'

    object_types = {
        'package': ObjType(l_('package'), 'package'),
        'function': ObjType(l_('function'), 'function'),
        'macro': ObjType(l_('macro'), 'macro'),
        'variable': ObjType(l_('variable'), 'variable'),
        'type': ObjType(l_('type'), 'type'),
        'method': ObjType(l_('method'), 'method'),
    }

    directives = {
        'package': CLCurrentPackage,
        'function': CLsExp,
        'genericfunction': CLsExp,
        'macro': CLsExp,
        'variable': CLsExp,
        'type': CLsExp,
        'method': CLsExp,
    }

    roles = {
        'symbol': CLXRefRole(),
    }
    initial_data = {
        'symbols': {},
    }

    def clear_doc(self, docname):
        for fullname, (fn, _) in self.data['symbols'].items():
            if fn == docname:
                del self.data['symbols'][fullname]

    def find_obj(self, env, name):
        """Find a Lisp symbol for "name", perhaps using the given package
        Returns a list of (name, object entry) tuples.
        """
        symbols = self.data['symbols']
        matches = []
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

    def resolve_xref(self, env, fromdocname, builder,
                     typ, target, node, contnode):
        matches = self.find_obj(env, target.upper())
        if not matches:
            return None
        elif len(matches) > 1:
            env.warn_node(
                'more than one target found for cross-reference '
                '%r: %s' % (target, ', '.join(match[0] for match in matches)),
                node)
        name, obj = matches[0]
        return make_refnode(builder, fromdocname, obj[0], name,
                            contnode, name)

    def get_symbols(self):
        for refname, (docname, type) in self.data['symbols'].iteritems():
            yield (refname, refname, type, docname, refname, 1)


def index_package(package, package_path, extra_args=""):
    """Call an external lisp program that will return a dictionary of
    doc strings for all public symbols."""
    lisp_script = path.join(path.dirname(path.realpath(__file__)),
                            "cldomain.sh")
    command = "%s --package %s --path %s" % (lisp_script, package,
                                             package_path)
    output = subprocess.check_output(command + extra_args, shell=True)
    output = "\n".join([line for line in output.split("\n")
                        if not line.startswith(";")])
    lisp_data = eval(output)
    doc_strings[package] = {}
    specializers[package] = {}
    for k, v in lisp_data.items():
        # extract doc strings
        doc_strings[package][k] = {}
        for type in TYPES:
            if not type in v:
                continue
            doc_strings[package][k][type] = re.sub(
                upper_symbols,
                "\g<1>:cl:symbol:`~\g<2>`\g<3>", v[type])

        # extract specializers
        if "specializers" in v:
            specializers[package][k] = v["specializers"]

    args[package] = {}

    def lower_symbols(text):
        if '"' in text:
            return text
        if text.startswith(package):
            return text[len(package) + 2:].lower()
        return text.lower()
    # extract arguments
    for k, v in lisp_data.items():
        if v["arguments"] == "NIL":
            args[package][k] = ""
        else:
            v_arg = v["arguments"].replace('(', ' ( ').replace(')', ' ) ')
            args[package][k] = " ".join(map(lower_symbols, v_arg.split(" ")))
    print args


def load_packages(app):
    if not app.config.lisp_packages:
        return
    for key, value in app.config.lisp_packages.iteritems():
        index_package(key.upper(), value)


def uppercase_symbols(app, docname, source):
    """For each line in a list replace all uppercase symbols with a
    sphinx references"""
    for i, line in enumerate(source):
        source[i] = re.sub(upper_symbols,
                           "\g<1>:cl:symbol:`~\g<2>`\g<3>", line)


def setup(app):
    app.add_domain(CLDomain)
    app.add_config_value('lisp_packages', {}, 'env')
    app.connect('builder-inited', load_packages)
    #app.connect('source-read', uppercase_symbols)
