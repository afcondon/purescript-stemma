#!/usr/bin/env bash
# Rebuild the demonstrator: embed the examples and Graphviz's drawings of
# them, compile, and bundle into public/. Run inside the Nix shell (spago,
# purs, esbuild and node on PATH), from anywhere.
#
#   site/build.sh           build into site/public/ (for a local server)
#   site/build.sh --docs    also copy the finished site into docs/ at the
#                           top of the repository, for GitHub Pages
#                           ("deploy from a branch", folder /docs), so that
#                           the published page needs no build on GitHub.
#
# The family tree's facts are not fetched here: they are kept in
# site/examples/fairport/ and refreshed only by running
# site/scripts/fairport.mjs by hand.
set -euo pipefail
here="$(cd "$(dirname "$0")" && pwd)"
cd "$here/.."
node site/scripts/bundle-examples.mjs
spago build -p graphviz-site
spago bundle -p graphviz-site --platform browser --module Site.Main --minify --outfile public/bundle.js   # relative to the package, site/

if [[ "${1:-}" == "--docs" ]]; then
  rm -rf docs
  mkdir -p docs
  cp site/public/index.html site/public/style.css site/public/bundle.js docs/
  cp -R site/public/graphviz docs/graphviz
  # Pages would otherwise run the folder through Jekyll, which is not needed.
  touch docs/.nojekyll
  echo "site copied to docs/"
fi
