#!/usr/bin/env bash
#
# Fail if stack.yaml lists a dependency by a path outside the repository.
#
# CI checks out this repository alone, so an extra-dep that points at a
# sibling checkout (such as ../haskoin-core) builds on the author's machine
# and nowhere else. Catching it here gives a clear message instead of a build
# failure deep inside Stack.
#
# Run from the root of the repository. The message uses the "::error" syntax
# of GitHub Actions, which shows it as an annotation on stack.yaml.

set -euo pipefail

# A list item whose value starts with "./", "../", "/" or "~".
local_path='^[[:space:]]*-[[:space:]]*(\.\.?/|/|~)'

if grep -nE "$local_path" stack.yaml; then
  message="stack.yaml refers to local paths outside the repository."
  message+=" Pin released versions (or a git commit) in extra-deps"
  message+=" before pushing."
  echo "::error file=stack.yaml::$message"
  exit 1
fi
