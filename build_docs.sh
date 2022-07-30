#!/bin/sh
set -e

(cd doc; make latexpdf)
(cd doc; make info)
(cd doc; make html)
