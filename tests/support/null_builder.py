"""Do syntax checks, but no writing."""

from typing import Any, Dict, Set

from docutils.nodes import Node
from sphinx.application import Sphinx
from sphinx.builders import Builder
from sphinx.locale import __


class NullBuilder(Builder):
    name = "null"
    epilog = __("The null builder generates no files.")

    allow_parallel = True
    root_nodes = []

    def init(self) -> None:
        pass

    def get_outdated_docs(self) -> Set[str]:
        return self.env.found_docs

    def get_target_uri(self, docname: str, typ: str = None) -> str:
        return ""

    def prepare_writing(self, docnames: Set[str]) -> None:
        pass

    def write_doc(self, docname: str, doctree: Node) -> None:
        self.root_nodes.append(doctree)

    def finish(self) -> None:
        pass


def setup(app: Sphinx) -> Dict[str, Any]:
    app.add_builder(NullBuilder)

    return {
        "version": "builtin",
        "parallel_read_safe": True,
        "parallel_write_safe": True,
    }
