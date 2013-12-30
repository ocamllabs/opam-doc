#!/usr/bin/env bash
# Serve the doc contents

DOC="$1"

if [ $DOC = "" ]; then
  DOC=$(opam config var root)/doc/doc
fi

cd $DOC
cohttp-server -p 8000 || true
python -m SimpleHTTPServer 
