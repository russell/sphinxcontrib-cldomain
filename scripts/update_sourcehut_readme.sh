#!/bin/bash

repo_id=217263
readme=README.html

rst2html5.py --template scripts/readme_template.txt README.rst > $readme

jq -sR '{
    "query": "mutation UpdateRepo($id: Int!, $readme: String!) {
      updateRepository(id: $id, input: { readme: $readme }) { id }
    }", "variables": {
      "id": '$repo_id',
      "readme": .
    } }' < $readme \
        | curl --oauth2-bearer $SOURCEHUT_TOKEN \
               -H "Content-Type: application/json" \
               -d@- https://git.sr.ht/query
