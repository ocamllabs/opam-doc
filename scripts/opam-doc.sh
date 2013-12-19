#!/bin/sh -e
# Create the documentation.

# Dirty work-around to not start the documention server
SERVE=opam-doc-serve
case "$1" in
    --version)
	echo "0.9.0"
	exit 0
	;;
    --no-server)
	SERVE="echo To run the server, use: opam-doc-serve"
	shift
	;;
esac

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
${SERVE} ${DOC}
