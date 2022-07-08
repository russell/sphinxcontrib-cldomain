python_sources(
    name="lib",
    sources=["sphinxcontrib/**/*.py"],
)

python_sources(
    name="tests",
    sources=["tests/**/*.py"],
)

resources(
    name="cldomain_files",
    sources=[
        "README.rst",
        "LICENSE",
    ]
)

python_distribution(
    name="sphinxcontrib-cldomain",
    dependencies=[
        "sphinxcontrib/cldomain:lisp_lib",
        ":lib",
        ":cldomain_files",
    ],
    provides=python_artifact(
        name="sphinxcontrib-cldomain",
        version="0.14",
        description = "Sphinx domain for Common Lisp",
        requires_python = ">=3.7",
        license = "GPLv3+",
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
            "documentation": "http://cldomain.russellsim.org/",
            "source": "https://github.com/russell/sphinxcontrib-cldomain",
            "tracker": "https://github.com/russell/sphinxcontrib-cldomain/issues",
        },
    ),
    wheel_config_settings={"--global-option": ["--python-tag", "py37.py38.py39"]},
)
