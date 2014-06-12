from os.path import expandvars
import os

from fabric.api import local, lcd, settings, env, puts
from fabric.contrib.console import confirm
from fabric.contrib.project import rsync_project
from fabric.decorators import task

env.use_ssh_config = True


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
def doc_server(port="8000"):
    """Serve documentation from localhost.

    @param port  Port to run server on.
    """
    with lcd("doc/html"):
        local("python -m SimpleHTTPServer %s" % port)


def push():
    local("git push")
    local("git push --tags")


@task
def pypi_upload():
    """upload a new version to pypi"""
    local("python setup.py egg_info --tag-build= --no-date sdist upload")


def inc_version():
    version = map(int, local("git describe --tags --abbrev=0",
                             capture=True).split('.'))
    version[1] += 1
    version = '.'.join(map(str, version))
    return version


def print_version():
    version = local('python setup.py --version', capture=True)
    puts('Version: %s' % version)


@task
def release_minor():
    generate_version(inc_version())


@task
def release_major():
    version = map(int, local("git describe --tags --abbrev=0",
                             capture=True).split('.'))
    version[0] += 1
    version[1] = 0
    version = '.'.join(map(str, version))
    generate_version(version)


def generate_version(version):
    filename = "sphinxcontrib/version.lisp-expr"
    with open(filename, "w") as version_file:
        version_file.write('"' + str(version) + '"')
    puts("Releasing %s" % version)
    if not confirm("Are you sure you want to release this version?"):
        puts("Aborted.")
        return
    local('git add CHANGELOG.rst')
    local("git add " + filename)
    message = "Released " + str(version)
    local("git commit -m '%s'" % message)
    local("git tag -a -m '" + message + "' %s" % version)


try:
    execfile(expandvars("$HOME/.projects.py"))

    @task
    def deploy():
        """deploy a prod version"""
        env = os.environ
        env["GOOGLE_ANALYTICS"] = "true"
        build()
        project_env = PROJECTS["sphinxcontrib-cldomain"]
        env.hosts = [project_env["host"]]
        assert project_env["remote_dir"]
        with settings(host_string=project_env["host"]):
            rsync_project(project_env["remote_dir"], "doc/html/", delete=True)
        clean()

    @task
    def deploy_release():
        """pypi upload, push, deploy"""
        # TODO should do an sdist and test install into a virtualenv.
        print_version()
        if not confirm("Are you sure you want to release this version?"):
            puts("Aborted.")
            return
        pypi_upload()
        push()
        deploy()

except:
    pass
