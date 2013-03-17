#!/bin/bash
# cldomain a Common Lisp domain for sphinx.
# Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
DIR=$(cd $(dirname "${BASH_SOURCE[0]}") && pwd)

set -x

sbcl --noinform --non-interactive \
    --load $DIR/cldomain-init.lisp \
    --eval "(pushnew \""${DIR}"/\" asdf:*central-registry* :test #'equal)" \
    --eval "(asdf:initialize-source-registry)" \
    --eval "(let ((*standard-output* *error-output*)) (ql:quickload 'sphinxcontrib.cldomain))" \
    --load $DIR/cldomain.lisp \
    --eval "(sphinxcontrib.cldomain:main)" $@
