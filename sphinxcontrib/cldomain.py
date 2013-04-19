# -*- coding: utf-8 -*-
# cldomain is a Common Lisp domain for the Sphinx documentation tool.
# Copyright (C) 2011-2013 Russell Sim <russell.sim@gmail.com>

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
import os
from os import path
import json
from collections import defaultdict
import subprocess
from StringIO import StringIO
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

__version__ = open(path.join(path.dirname(__file__),
                   "version.lisp-expr")).read().strip('"')

ALL_TYPES = ["macro", "function", "genericFunction",
             "setf", "variable", "type"]
upper_symbols = re.compile("([^a-z\s\"`]*[A-Z]{2,}[^a-z\s\"`:]*)($|\s)")

DOC_STRINGS = defaultdict(dict, {})
TYPES = defaultdict(dict, {})
ARGS = defaultdict(dict, {})
METHODS = defaultdict(dict, {})
SLOTS = defaultdict(dict, {})
USED_SYMBOLS = defaultdict(dict, {})

lambda_list_keywords = ["&allow-other-keys", "&key",
                        "&rest", "&aux", "&optional"]


def record_use(package, symbol_name, objtype):
    """record unused package symbols."""
    symbol = symbol_name.upper()
    USED_SYMBOLS[package].setdefault(symbol, []).append(objtype)


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


def parse_specializer_symbol(symbol, package):
    """parse symbols, for specializers"""
    symbol = symbol.upper()
    if symbol.startswith(":"):
        return "KEYWORD" + symbol
    # TODO (RS) this needs to be smarter what happens if there is an
    # internal symbol instead of an external one?
    if ":" not in symbol:
        return package + ":" + symbol
    return symbol


def resolve_string(state_machine, package, symbol, objtype, specializer=None):
    """
    Resolve a symbols doc string. Will raise KeyError if the
    symbol can't be found.
    """
    if objtype == "method":
        spec = specializer[0].split(" ")[1:]
        method_doc = METHODS[package].get(symbol, {})
        key = tuple([parse_specializer_symbol(sym, package)
                     for sym in spec])
        if key not in method_doc:
            state_machine.reporter.warning("Can't find method %s:%s specializer %s" %
                                           (package, symbol, spec))
        return method_doc.get(key, "")

    possible_strings = DOC_STRINGS[package][symbol]

    # XXX This isn't the best, the objtype is generic but the
    # docstring will be under genericFunction because of the JSON
    # encoder and changing the directive name doesn't seem to help
    # either.
    if objtype == "generic":
        objtype = "genericFunction"
    string = possible_strings.get(objtype, "")
    return string


class desc_clparameterlist(addnodes.desc_parameterlist):
    """Node for a common lisp parameter list."""
    child_text_separator = ' '

# v is short for visit
# d is short for depart


def v_clparameterlist(self, node):
    self.first_param = 1
    self.param_separator = node.child_text_separator


def d_clparameterlist(self, node):
    pass


def v_latex_clparameterlist(self, node):
    self.body.append('}{')
    self.first_param = 1
    self.param_separator = node.child_text_separator


def d_latex_clparameterlist(self, node):
    self.body.append('}{')


class desc_clparameter(addnodes.desc_parameter):
    """Node for a common lisp parameter item."""


def d_clparameter(self, node):
    pass


def v_html_clparameter(self, node):
    if not self.first_param:
        self.body.append(self.param_separator)
    else:
        self.first_param = 0
    if node.hasattr('lambda_keyword'):
        self.body.append('<em class="lambda_keyword">')
    elif node.hasattr('keyword'):
        self.body.append('<em class="keyword">')
    elif not node.hasattr('noemph'):
        self.body.append('<em>')


def d_html_clparameter(self, node):
    if node.hasattr('lambda_keyword'):
        self.body.append('</em>')
    elif node.hasattr('keyword'):
        self.body.append('</em>')
    elif not node.hasattr('noemph'):
        self.body.append('</em>')


def v_latex_clparameter(self, node):
    if not self.first_param:
        self.body.append(self.param_separator)
    else:
        self.first_param = 0
    if not node.hasattr('noemph'):
        self.body.append(r'\emph{')


def d_latex_clparameter(self, node):
    if not node.hasattr('noemph'):
        self.body.append('}')


def v_texinfo_clparameter(self, node):
    if not self.first_param:
        self.body.append(self.param_separator)
    else:
        self.first_param = 0
    text = self.escape(node.astext())
    # replace no-break spaces with normal ones
    text = text.replace(u' ', '@w{ }')
    self.body.append(text)
    raise nodes.SkipNode


def v_text_clparameter(self, node):
    if not self.first_param:
        self.add_text(self.param_separator)
    else:
        self.first_param = 0
    self.add_text(node.astext())
    raise nodes.SkipNode


def specializer(sexp, state):
    result = StringIO()
    for atom in sexp:
        if atom.startswith("KEYWORD:"):
            result.write("(EQL :%s)" % atom.split(":")[-1])
        else:
            result.write(":cl:symbol:`~%s`" % atom)
        result.write(" ")

    node = nodes.list_item()
    result.seek(0)
    lines = string2lines(result.read())
    state.nested_parse(StringList(lines), 0, node)
    return node


class SpecializerField(Field):
    """
    """
    is_grouped = True
    list_type = nodes.bullet_list

    def __init__(self, name, names=(), label=None, rolename=None,
                 can_collapse=False):
        Field.__init__(self, name, names, label, True, rolename)
        self.can_collapse = can_collapse

    def make_field(self, domain, items):
        fieldname = nodes.field_name('', self.label)
        listnode = self.list_type()
        for content in items:
            par = nodes.paragraph()
            par += content
            listnode += nodes.list_item('', par)
        fieldbody = nodes.field_body('', listnode)
        return nodes.field('', fieldname, fieldbody)


class SEXP(object):
    def __init__(self, sexp, show_defaults=False):
        if not isinstance(sexp, list):
            self.sexp = _read(sexp)
        else:
            self.sexp = sexp
        self.show_defaults = show_defaults

    def as_parameterlist(self, function_name):
        return self.render_parameterlist(prepend_node=function_name)

    def render_parameterlist(self, signode=None, prepend_node=None, sexp=None):
        desc_sexplist = desc_clparameterlist()
        if prepend_node:
            desc_sexplist.append(prepend_node)
        if signode:
            signode.append(desc_sexplist)
        symbol = False
        for atom in sexp or self.sexp:
            if isinstance(atom, list):
                if self.show_defaults:
                    symbol = self.render_parameterlist(signode=desc_sexplist, sexp=atom)
                else:
                    symbol = self.render_atom(atom[0], desc_sexplist)
            else:
                symbol = self.render_atom(atom, desc_sexplist)
        return desc_sexplist

    def render_atom(self, token, signode, noemph=True):
        "add syntax hi-lighting to interesting atoms"

        param = desc_clparameter(token, token)
        if token.lower() in lambda_list_keywords:
            param["lambda_keyword"] = True
        if token.startswith(":"):
            praam["keyword"] = True
        signode.append(param)


class CLsExp(ObjectDescription):

    doc_field_types = [
        GroupedField('parameter', label=l_('Parameters'),
                     names=('param', 'parameter', 'arg', 'argument',
                            'keyword', 'kwparam')),
        Field('returnvalue', label=l_('Returns'), has_arg=False,
              names=('returns', 'return')),
    ]

    option_spec = {
        'nodoc': bool_option,
        'noindex': bool_option,
    }

    def handle_signature(self, sig, signode):
        symbol_name = []
        type = []
        package = self.env.temp_data.get('cl:package')
        objtype = self.get_signature_prefix(sig)
        sig_split = sig.split(" ")
        sig = sig_split[0]
        args = sig_split[1:]
        signode.append(addnodes.desc_annotation(objtype, objtype))
        lisp_args = ARGS[package].get(sig.upper(), "")

        if lisp_args.strip():
            function_name = addnodes.desc_name(sig, sig + " ")
        else:
            function_name = addnodes.desc_name(sig, sig)

        if not lisp_args.strip() and self.objtype in ["function"]:
            lisp_args = "()"
        if lisp_args.strip():
            sexp = SEXP(lisp_args, self.env.app.config.cl_show_defaults)
            arg_list = sexp.as_parameterlist(function_name)
            signode.append(arg_list)
        else:
            signode.append(function_name)

        # Add Slots
        slots = SLOTS[package].get(sig.upper())
        if slots:
            # TODO add slot details if describing a class
            pass

        symbol_name = sig
        if not symbol_name:
            raise Exception("Unknown symbol type for signature %s" % sig)
        record_use(package, symbol_name, self.objtype)
        return objtype.strip(), symbol_name.upper()

    def get_index_text(self, name, type):
        return _('%s (Lisp %s)') % (name.lower().split(":")[-1], type)

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

        if not type == "method":
            indextext = self.get_index_text(name, type)
            if indextext:
                self.indexnode['entries'].append(('single', indextext, name, ''))

    def run_add_doc(self, result):
        package = self.env.temp_data.get('cl:package')
        name = self.names[0][1]
        description = result[1][-1]
        node = addnodes.desc_content()
        try:
            string = resolve_string(self.state_machine, package,
                                    name, self.objtype,
                                    self.arguments)
        except KeyError:
            string = ""
            self.state_machine.reporter.warning("Can't find symbol %s:%s" %
                                                (package, name))
        if not string:
            return
        lines = string2lines(string)
        self.state.nested_parse(StringList(lines), 0, node)
        # result[1] is the content node,
        # result[1][0] is the signature line
        # result[1][1] is the fieldlist and description
        # result[1][1][0] is the fieldlist
        if (result[1][1].children and
            isinstance(description[0], nodes.field_list)):
            cresult = description.deepcopy()
            target = description
            target.clear()
            target.append(cresult[0])
            target.extend(node)
            target.extend(cresult[1:])
        else:
            cresult = description.deepcopy()
            target = description
            target.clear()
            target.extend(node)
            target.extend(cresult)
        return result

    def run_add_specializers(self, result):
        package = self.env.temp_data.get('cl:package')
        name = self.names[0][1]
        description = result[1][-1]
        specializers = METHODS[package].get(name, {}).keys()
        if specializers:
            description.append(nodes.paragraph(text="Specializes"))
            spec = nodes.bullet_list()
            spec += [specializer(s, self.state) for s in specializers]
            description.children.append(spec)
        return result

    def run(self):
        result = super(CLsExp, self).run()
        if "nodoc" not in self.options:
            self.run_add_doc(result)
        if self.objtype == "generic" and "nospecializers" not in self.options:
            self.run_add_specializers(result)
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
                symbol = title[1:].split(':')
                package = symbol[0]
                title = symbol[-1]
                if target[0] == ":":
                    title = ":" + title
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
        'generic': ObjType(l_('generic'), 'generic'),
        'method': ObjType(l_('method'), 'method'),
    }

    directives = {
        'package': CLCurrentPackage,
        'function': CLsExp,
        'generic': CLsExp,
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


def code_regions(text):
    io = StringIO(text)
    output = StringIO()
    indent = False
    for line in io:
        if indent is False and (line.startswith(" ") or line.startswith("\t")):
            output.write("\n.. code-block:: common-lisp\n\n")
            indent = True
        if indent is True and not (line.startswith(" ") or line.startswith("\t")):
            indent = False
        output.write(line)
    output.seek(0)
    return output.read()


def index_package(package, package_path, quicklisp):
    """Call an external lisp program that will return a dictionary of
    doc strings for all public symbols."""
    cl_launch_exe = [which("cl-launch")[0]]
    cl_launch_command = cl_launch_args()
    cldomain_args = ["--", "--package", package, "--path", package_path]

    env = {"CLDOMAIN": path.abspath(path.dirname(__file__)) + "/",
           "QUICKLISP": quicklisp}

    output = subprocess.check_output(cl_launch_exe + cl_launch_command + cldomain_args,
                                     env=env)
    output = "\n".join([line for line in output.split("\n")
                        if not line.startswith(";")])

    lisp_data = json.loads(output)
    for k, v in lisp_data.items():

        # extract doc strings
        DOC_STRINGS[package][k] = {}
        for type in ALL_TYPES:
            if not type in v:
                continue
            # enable symbol references for symbols
            DOC_STRINGS[package][k][type] = code_regions(v[type])

        # extract methods
        if "methods" in v:
            def parse_method(method):
                sexp = []
                for atom in json.loads(method):
                    if atom.startswith("("):
                        eql = _read(atom)
                        sexp.append(eql[-1])
                    else:
                        sexp.append(atom)
                return tuple(sexp)

            def parse_doc(doc):
                if doc is None:
                    doc = ""
                return code_regions(doc)

            methods = dict([(parse_method(method), parse_doc(doc))
                            for method, doc in v["methods"].items()])
            METHODS[package][k] = methods

        # extract slots
        if "slots" in v:
            SLOTS[package][k] = v["slots"]

    def lower_symbols(text):
        if '"' in text:
            return text
        if len(text.split("::")) > 1:
            spackage, symbol = text.split("::")
        elif len(text.split(":")) > 1:
            spackage, symbol = text.split(":")
        else:
            spackage = ""
        if spackage.lower() == package.lower():
            return symbol.lower()
        return text.lower()
    # extract arguments
    for k, v in lisp_data.items():
        if not v.get("arguments"):
            pass
        elif v["arguments"] == "NIL":
            ARGS[package][k] = ""
        else:
            v_arg = v["arguments"].replace('(', ' ( ').replace(')', ' ) ')
            ARGS[package][k] = " ".join(map(lower_symbols, v_arg.split(" ")))


def load_packages(app):
    if not app.config.cl_packages:
        return
    for key, value in app.config.cl_packages.iteritems():
        index_package(key.upper(), value, app.config.cl_quicklisp)


def uppercase_symbols(app, docname, source):
    """For each line in a list replace all uppercase symbols with a
    sphinx references"""
    for i, line in enumerate(source):
        source[i] = re.sub(upper_symbols,
                           ":cl:symbol:`~\g<1>`\g<2>", line)


def list_unused_symbols(app, exception):
    if exception:
        return
    # TODO (RS) this initial implementation will not be able to detect
    # if each method specialisation has been used.
    for p, sym_doc in DOC_STRINGS.items():
        for s, docs in sym_doc.items():
            for objtype in docs.keys():
                if s in USED_SYMBOLS[p]:
                    if objtype == "genericFunction":
                        objtype = "generic"
                    if objtype not in USED_SYMBOLS[p][s]:
                        app.warn("Unused symbol doc %s:%s type %s" %
                                           (p, s, objtype))
                else:
                    app.warn("Unused symbol doc %s:%s type %s" %
                                           (p, s, objtype))


def setup(app):
    app.add_domain(CLDomain)
    app.add_node(desc_clparameterlist,
                 html=(v_clparameterlist, d_clparameterlist),
                 latex=(v_latex_clparameterlist, d_latex_clparameterlist),
                 texinfo=(v_clparameterlist, d_clparameterlist),
                 text=(v_clparameterlist, d_clparameterlist))
    app.add_node(desc_clparameter,
                 html=(v_html_clparameter, d_html_clparameter),
                 latex=(v_latex_clparameter, d_latex_clparameter),
                 texinfo=(v_texinfo_clparameter, d_clparameter),
                 text=(v_text_clparameter, d_clparameter))
    app.add_config_value('cl_packages', {}, 'env')
    app.add_config_value('cl_quicklisp', "", 'env')
    app.add_config_value('cl_show_defaults', False, True)
    app.connect('builder-inited', load_packages)
    app.connect('build-finished', list_unused_symbols)
    #app.connect('source-read', uppercase_symbols)


def which(name, flags=os.X_OK):
    """https://twistedmatrix.com/trac/browser/tags/releases/twisted-8.2.0/twisted/python/procutils.py"""
    result = []
    exts = filter(None, os.environ.get('PATHEXT', '').split(os.pathsep))
    path = os.environ.get('PATH', None)
    if path is None:
        return []
    for p in os.environ.get('PATH', '').split(os.pathsep):
        p = os.path.join(p, name)
        if os.access(p, flags):
            result.append(p)
        for e in exts:
            pext = p + e
            if os.access(pext, flags):
                result.append(pext)
    return result


def cl_launch_args():
    quicklisp="""
(let ((quicklisp-init (merge-pathnames (make-pathname :name "setup"
                                                      :type "lisp")
                                       (concatenate 'string (asdf/os:getenv "QUICKLISP")
                                                    "/"))))
  (if (probe-file quicklisp-init)
      (load quicklisp-init)
      (error "Can't Find Quicklisp at ~a~%" quicklisp-init)))
"""

    system = """
(push (pathname (concatenate 'string (asdf/os:getenv \"CLDOMAIN\") \"/\"))
                             asdf:*central-registry*)
"""

    quickload = """
(let ((*standard-output* *error-output*))
  (quicklisp:quickload 'sphinxcontrib.cldomain))
"""

    return ["--init", quicklisp,
            "--init", system,
            "--init", "(asdf:initialize-source-registry)",
            "--init", "(require 'quicklisp)",
            "--init", quickload,
            "--init", "(sphinxcontrib.cldomain:main)"]
