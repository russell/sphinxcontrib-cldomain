# -*- coding: utf-8 -*-
"""
    sphinx.domains.cl
    ~~~~~~~~~~~~~~~~~

    The Common Lisp domain

    :copyright: Copyright 2011 by Russell Sim
    :license: GPL, see LICENSE for details.


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


upper_symbols = re.compile("(^|\s)([^a-z\s\"]*[A-Z]{2,}[^a-z\s\"]*)($|\s)")

doc_strings = {}


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

lisp_types = {
    "defun": "function",
    "defmacro": "macro",
    "defparameter": "parameter",
}


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
        sexp = _read(sig)
        symbol_name = []
        type = []

        def render_sexp(sexp, signode):
            desc_sexplist = addnodes.desc_parameterlist()
            desc_sexplist.child_text_separator = ' '
            signode.append(desc_sexplist)
            symbol = False
            for atom in sexp:
                if isinstance(atom, list):
                    render_sexp(atom, desc_sexplist)
                elif symbol:
                    desc_sexplist.append(addnodes.desc_name(atom, atom))
                    symbol = False
                    symbol_name.append(atom)
                else:
                    symbol = render_atom(atom, desc_sexplist)

        def render_atom(token, signode, noemph=True):
            "add syntax hi-lighting to interesting atoms"

            if token.startswith("&") or token.startswith(":"):
                signode.append(addnodes.desc_parameter(token, token))
            elif token in lisp_types:
                signode.append(addnodes.desc_annotation(token, token))
                type.append(token)
                return True
            else:
                signode.append(addnodes.desc_parameter(token, token))

        render_sexp(sexp, signode)
        if not symbol_name:
            raise Exception("Unknown symbol type for signature %s" % sig)
        return type[0], symbol_name[0].upper()

    def get_index_text(self, name, type):
        if type in lisp_types:
            return _('%s (Lisp %s)') % (name, lisp_types[type])
        else:
            return ''

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
            string = doc_strings.get(package).get(self.names[0][1], "")
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
        index_package(self.arguments[0].upper())
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
        'parameter': ObjType(l_('parameter'), 'parameter'),
    }

    directives = {
        'package': CLCurrentPackage,
        'function': CLsExp,
        'macro': CLsExp,
        'parameter': CLsExp,
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


def index_package(package, extra_args=""):
    """Call an external lisp program that will return a dictionary of
    doc strings for all public symbols."""
    lisp_script = path.join(path.dirname(path.realpath(__file__)),
                            "cldomain.lisp")
    command = "sbcl --noinform --disable-ldb --lose-on-corruption --end-runtime-options --noprint --non-interactive --load %s" % lisp_script
    output = subprocess.check_output(
        command + extra_args + " --eval '(json-documentation :%s)'" % package,
        shell=True)
    output = "\n".join([line for line in output.split("\n")
                        if not line.startswith(";")])
    doc_strings[package] = eval(output)
    for k, v in doc_strings[package].items():
        doc_strings[package][k] = re.sub(upper_symbols,
                                         "\g<1>:cl:symbol:`~\g<2>`\g<3>", v)


def load_packages(app):
    if not hasattr(app.config, "lisp_packages"):
        return
    for key, value in app.config.lisp_packages.iteritems():
        index_package(key)


def uppercase_symbols(app, docname, source):
    """For each line in a list replace all uppercase symbols with a
    sphinx references"""
    for i, line in enumerate(source):
        source[i] = re.sub(upper_symbols,
                           "\g<1>:cl:symbol:`~\g<2>`\g<3>", line)


def setup(app):
    app.add_domain(CLDomain)
    app.connect('builder-inited', load_packages)
    app.connect('source-read', uppercase_symbols)
