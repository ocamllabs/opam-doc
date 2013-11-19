#!/usr/bin/env bash
# Rebuild the cmd/cmt archive in ~/.opam/<switch>/opamdoc
# Copies all the cmt/cmti/cmd files found in the OPAM build
# dir into a single directory structure, separated by MD5
# to keep files distinct.

set -e
#dry=echo
SWITCH=$(opam switch show)
if [ "${SWITCH}" = "system" ]; then
  echo Must be using a custom OPAM switch for this to work.
  exit 1
fi

OPAMDOC=${OPAMDOC:-opamdoc}

BASE="$(dirname $(dirname $(ocamlc -where)))"
BUILD=${BASE}/build
DOC=${BASE}/opamdoc
HTML=${BASE}/opamhtml

rm -rf ${HTML}
mkdir -p ${HTML}

# Grab the build dir in reverse mtime order to get the
# ordering of generation "correct".
PKGS=$(ls -1tr $BUILD)

rm -rf $HTML
mkdir -p $HTML
cd $HTML
for pkg in ${PKGS}; do
  fs="$(find $DOC/$pkg -type f)"
  if [ "$fs" != "" ]; then
    name=$(echo $pkg | awk -F. '{print $1}')
    echo $pkg $name
    $dry $OPAMDOC -p $name $fs
  fi
done
