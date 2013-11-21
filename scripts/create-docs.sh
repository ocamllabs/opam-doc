#!/usr/bin/env bash
# Create the documentation.

BASE=`pwd`

OPAMDOC=${BASE}/opam-doc

OPAM=${BASE}/opam
DATA=${BASE}/data
DOC=${BASE}/$1

# Grab the build dir in reverse mtime order to get the
# ordering of generation "correct".
PKGS=$(ls -1tr $OPAM/system/build)

rm -rf ${DOC}
mkdir ${DOC}

cd ${DOC}
for pkg in ${PKGS}; do
  fs="$(find ${DATA}/$pkg -type f)"
  if [ "$fs" != "" ]; then
    name=$(echo $pkg | awk -F. '{print $1}')
    echo "Generating documentation for $name"
    ${OPAMDOC} --filter-pervasives -p $name --base "$2" $fs
  fi
done
