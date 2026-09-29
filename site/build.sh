#!/bin/sh
# Builds the demo site: x-ocaml in the release profile, and its docs, put
# together with the example pages and their elements in one directory,
# ready to serve as it is:
#
#   site/build.sh [DIR]              (DIR is _site by default)
#   cd DIR && python3 -m http.server
#
# then open http://localhost:8000/. The pages are under examples/, as in
# the repository, beside the scripts they load with ../x-ocaml.js; the
# rendered docs are under doc/.
#
# The docs are built by odoc_driver from what dune installs, so opam must
# know the x-ocaml package: once, `opam install .` (or `opam pin add -n .`).
# With --remap, odoc_driver renders only x-ocaml's pages, and links to the
# packages it depends on point elsewhere instead of to pages of their own.
set -eu

root=$(cd "$(dirname "$0")/.." && pwd)
out=${1:-"$root/_site"}
case $out in /*) ;; *) out="$(pwd)/$out" ;; esac

cd "$root"
opam exec -- dune build --profile=release ./x-ocaml.js ./x-ocaml.worker.js
opam exec -- dune build @install
docs=$(mktemp -d)
trap 'rm -rf "$docs"' EXIT
opam exec -- dune exec -- odoc_driver x-ocaml --remap \
  --odoc-dir "$docs/odoc" --odocl-dir "$docs/odocl" --index-dir "$docs/index" \
  --mld-dir "$docs/mld" --html-dir "$docs/html" > "$docs/log" 2>&1 ||
  { cat "$docs/log"; exit 1; }

rm -rf "$out"
mkdir -p "$out/examples/elements" "$out/doc"
cp _build/default/x-ocaml.js _build/default/x-ocaml.worker.js "$out/"
cp examples/*.html "$out/examples/"
cp examples/*.css "$out/examples/"
cp examples/elements/*.js "$out/examples/elements/"
cp -R "$docs/html/." "$out/doc/"
cp site/index.html "$out/index.html"
# What dune builds is read-only; the copies need not be.
chmod -R u+w "$out"

echo "The site is in $out: cd there and run python3 -m http.server"
