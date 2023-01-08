python_sources(
    name="lib",
    sources=["sphinxcontrib/**/*.py"],
)

python_sources(
    name="tests",
    sources=["tests/**/*.py"],
)

resources(
    name="docs",
    sources=[
        "README.rst",
        "CHANGELOG.rst",
        "LICENSE",
    ],
)

python_distribution(
    name="sphinxcontrib-cldomain",
    dependencies=[
        "sphinxcontrib/cldomain:lisp_lib",
        ":lib",
        ":docs",
    ],
    provides=python_artifact(
        name="sphinxcontrib-cldomain",
        version="0.0.0",
    ),
)
