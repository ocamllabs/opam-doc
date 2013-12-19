#!/bin/sh -e
# Serve the doc contents

cd $1
python -m SimpleHTTPServer
