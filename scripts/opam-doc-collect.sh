#!/usr/bin/env bash
# Rebuild the cmd/cmt archive. Installs the package list and
# copies all the cmt/cmti/cmd files found in the OPAM build
# dir into a single directory structure, separated by MD5
# to keep files distinct.
set -e

BUILD_DIR=$(opam config var root)/doc/build
DATA_DIR=$(opam config var root)/doc/data-doc

IMPORTS=$(opam list -s)

rm -rf ${DATA_DIR}
mkdir ${DATA_DIR}

PKGS=$(find ${BUILD_DIR} -mindepth 1 -maxdepth 1 -type d)

for pkg in ${PKGS}; do
  pkgname=$(basename $pkg)
  echo $pkgname
  mkdir ${DATA_DIR}/$pkgname
  cd $pkg
  FILES=$(find . -type f \( -name \*.cmd -o -name \*.cmdi -o -name \*.cmt -o -name \*.cmti \) )
  for f in ${FILES}; do
    rsync -r $f ${DATA_DIR}/$pkgname
  done
done
