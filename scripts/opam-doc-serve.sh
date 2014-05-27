#!/usr/bin/env bash
# Serve the doc contents

DOC="$1"

if [ -z "$DOC" ]; then
  DOC=$(opam config var root)/doc/doc
fi

function installed {
  local cmd=$1
  command -v ${cmd} >/dev/null
}

if installed cohttp-server-async; then
  SERVER="cohttp-server-async -p 8000"
elif installed cohttp-server; then
  SERVER="cohttp-server -p 8000"
elif installed python2; then
  SERVER="python2 -m SimpleHTTPServer"
elif installed python3; then
  SERVER="python3 -m http.server"
elif installed python; then
  SERVER="python -m SimpleHTTPServer"
else
  echo "No supported HTTP server found. Please install cohttp:"
  echo "  opam install async cohttp"
  echo
  exit 1
fi

cd $DOC
exec $SERVER 2>/dev/null

