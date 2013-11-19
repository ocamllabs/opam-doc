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

function calc_md5_for_file()
{
  if builtin command -v md5 > /dev/null; then
    md5=$(cat $1 | md5)
  elif builtin command -v md5sum > /dev/null ; then
    md5=$(cat $1 | md5sum | awk '{print $1}')
  else
    echo "Neither md5 nor md5sum were found in the PATH"
    exit 1
  fi
}

BASE="$(dirname $(dirname $(ocamlc -where)))"
BUILD=${BASE}/build
DOC=${BASE}/opamdoc
rm -rf ${DOC}
mkdir -p ${DOC}

PKGS=$(find ${BUILD}/ -mindepth 1 -maxdepth 1 -type d)
echo ${PKGS}
for pkg in ${PKGS}; do
  pkgname=$(basename $pkg)
  echo $pkgname
  mkdir -p ${DOC}/$pkgname
  CMDS=$(find ${pkg} -type f -name \*.cmd)
  for cmd in ${CMDS}; do
    d=$(dirname $cmd)
    calc_md5_for_file "$cmd";
    f=$(basename $cmd .cmd)
    mkdir -p ${DOC}/$pkgname/$md5
    r=${DOC}/$pkgname/$md5/$f
    $dry cp $d/$f.cmd $r.cmd
    if [ -e $d/$f.cmdi ]; then
      $dry cp $d/$f.cmdi $r.cmdi
    fi
    if [ -e $d/$f.cmt ]; then
      $dry cp $d/$f.cmt $r.cmt
    fi
    if [ -e $d/$f.cmti ]; then
      $dry cp $d/$f.cmti $r.cmti
    fi
  done
done
