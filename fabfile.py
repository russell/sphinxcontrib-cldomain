from fabric.api import local, lcd, abort
from fabric.decorators import task


def clean():
    with lcd("doc"):
        local("rm -rf html/")


@task
def build():
    """build the documentation"""
    clean()
    with lcd("doc"):
        local("sphinx-build -b html -E . html")


@task
def pypi_upload():
    """upload a new version to pypi"""
    local("python setup.py egg_info --tag-build= --no-date sdist upload")
