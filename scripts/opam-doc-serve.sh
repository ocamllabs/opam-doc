#!/bin/sh -e
# Serve the doc contents

cd $1

cohttp-server -p 8000 || true
python -m SimpleHTTPServer 
