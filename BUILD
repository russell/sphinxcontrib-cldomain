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
    ]
)

python_distribution(
    name="sphinxcontrib-cldomain",
    dependencies=[
        "sphinxcontrib/cldomain:lisp_lib",
        ":lib",
        ":docs",
    ],
    long_description_path = "README.rst",
    provides=python_artifact(
        name="sphinxcontrib-cldomain",
        version="0.15",
        description = "Sphinx domain for Common Lisp",
        requires_python = ">=3.7",
        license = "GPLv3+",
        long_description_content_type = "text/x-rst",
        readme = { "file": "README.rst", "content_type": "text/x-rst" },
        authors = [
            { "name": "Russell Sim", "email": "russell.sim@gmail.com" }
        ],
        home_page = "https://github.com/russell/sphinxcontrib-cldomain",
        classifiers = [
            "Development Status :: 4 - Beta",
            "Environment :: Console",
            "Environment :: Web Environment",
            "Intended Audience :: Developers",
            "License :: OSI Approved :: GNU General Public License v3 or later (GPLv3+)",
            "Natural Language :: English",
            "Operating System :: OS Independent",
            "Programming Language :: Python :: 3.7",
            "Programming Language :: Python :: 3.8",
            "Programming Language :: Python :: 3.9",
            "Topic :: Documentation",
            "Topic :: Utilities",
        ],

        urls = {
            "documentation": "https://russell.github.io/sphinxcontrib-cldomain/",
            "source": "https://github.com/russell/sphinxcontrib-cldomain",
            "tracker": "https://github.com/russell/sphinxcontrib-cldomain/issues",
        },
    ),
    wheel_config_settings={"--global-option": ["--python-tag", "py37.py38.py39"]},
)
