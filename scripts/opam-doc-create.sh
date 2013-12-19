#!/usr/bin/env bash
# Create the documentation.
set -e

OPAMDOC_INDEX=opam-doc-index
BUILD_DIR=$(opam config var root)/doc/build
DATA_DIR=$(opam config var root)/doc/data-doc
DOC_DIR=$(opam config var root)/doc/doc

# Grab the build dir in reverse mtime order to get the
# ordering of generation "correct".
PKGS=$(ls -1tr ${BUILD_DIR})

rm -rf ${DOC_DIR}
mkdir ${DOC_DIR}

cd ${DOC_DIR}
for pkg in ${PKGS}; do
  fs="$(find ${DATA_DIR}/$pkg -type f)"
  if [ "$fs" != "" ]; then
    name=$(echo $pkg | awk -F. '{print $1}')
    echo "Generating documentation for $name"
    descr=`opam info "$name" -f description | head -1`
    ${OPAMDOC_INDEX} --filter-pervasives -p "$name" -descr "$descr" --base "http://127.0.0.1:8000" $fs
  fi
done
