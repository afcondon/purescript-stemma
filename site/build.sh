#!/usr/bin/env bash
# Rebuild the demonstrator: embed the examples and Graphviz's drawings of
# them, compile, and bundle into public/. Run inside the Nix shell (spago,
# purs, esbuild and node on PATH), from anywhere.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/.."
node site/scripts/bundle-examples.mjs
spago build -p graphviz-site
spago bundle -p graphviz-site --platform browser --module Site.Main --outfile public/bundle.js   # relative to the package, site/
