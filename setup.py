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

from setuptools import setup, find_packages
from os import path

changelog_header = """
Changelog
=========

"""

desc_file = path.join(path.dirname(__file__), "README.rst")
changelog_file = path.join(path.dirname(__file__), "CHANGELOG.rst")
description = open(desc_file).read() + changelog_header + open(changelog_file).read()

__version__ = open(path.join(path.dirname(__file__),
                             "sphinxcontrib",
                             "version.lisp-expr")).read().strip('"')

requires = ['Sphinx>=0.6']

setup(
    name='sphinxcontrib-cldomain',
    version=__version__,
    url='http://cldomain.russellsim.org/',
    download_url='http://pypi.python.org/pypi/sphinxcontrib-cldomain',
    license='GPL',
    author='Russell Sim',
    author_email='russell.sim@gmail.com',
    description='Sphinx domain for Common Lisp',
    long_description=description,
    zip_safe=False,
    classifiers=[
        'Development Status :: 4 - Beta',
        'Environment :: Console',
        'Environment :: Web Environment',
        'Intended Audience :: Developers',
        'License :: OSI Approved :: BSD License',
        'Operating System :: OS Independent',
        'Programming Language :: Python',
        'Topic :: Documentation',
        'Topic :: Utilities',
    ],
    platforms='any',
    packages=find_packages(),
    include_package_data=True,
    install_requires=requires,
    namespace_packages=['sphinxcontrib'],
)
