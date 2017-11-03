#!/bin/bash

# .gitignore sucks, so use this to add files to the repo

for f in $(find . -type f | grep -Ei ".(*\.(asm|b|c|cpp|d|f|for|h|hs|java|js|lisp|lua|pas|pl|php|pro|py|r|rs|sb2|sh)|Makefile|README.*)$"); do
  git add "$f"
done

git status
