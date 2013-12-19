#!/bin/sh -e
# Create the documentation.

PACKAGES=$*
DOC=$(opam config var root)/doc/doc

# Install the packages and keep the build dirs
echo "[1/4] Installing the packages"
opam install ${PACKAGES} -y -b --switch=doc

# Collect all the .cmd files in the build dirs
echo "[2/4] Collect the binary documentation files"
opam-doc-collect

# Build the HTML pages
echo "[3/4] Build the HTML pages"
opam-doc-create

# Serve the contents
echo "[4/4] Serve the contents of ${DOC} at http://127.0.0.1:8000"
opam-doc-serve ${DOC}
