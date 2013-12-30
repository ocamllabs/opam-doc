#!/usr/bin/env bash
# Create the documentation.
set -e

# Dirty work-around to not start the documention server
SERVE=opam-doc-serve

while getopts "nv" opt; do
  case $opt in
  v) echo 0.9.3; exit 0 ;;
  n) NOSERVE=1 ;;
  *) echo "Usage: $0 <pkgs> [-n]"; exit 0 ;;
  esac
done
shift $(($OPTIND - 1))

PACKAGES=$*
DOC=$(opam config var root)/doc/doc
BIN=$(opam config var bin)
BINDOC=$(opam config var root)/doc/bin

# Creating a 'doc' switch if needed
opam switch doc -A system --no-switch
cp ${BIN}/opam-doc-ocamlc ${BINDOC}/ocamlc
cp ${BIN}/opam-doc-ocamlc.opt ${BINDOC}/ocamlc.opt
cp ${BIN}/bin-doc ${BINDOC}/bin-doc

# Install the packages and keep the build dirs
echo "[1/4] Installing the packages: $PACKAGES"
opam config env --switch=doc
opam install ${PACKAGES} -y -b --switch=doc --verbose

# Collect all the .cmd files in the build dirs
echo "[2/4] Collect the binary documentation files"
opam-doc-collect

# Build the HTML pages
echo "[3/4] Build the HTML pages"
opam-doc-create

# Serve the contents
if [ "$NOSERVE" = "1" ]; then
  echo "To run server use: opam doc-serve"
else
  echo "[4/4] Serve the contents of ${DOC} at http://127.0.0.1:8000"
  ${SERVE} ${DOC}
fi
