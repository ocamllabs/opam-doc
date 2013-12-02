#!/usr/bin/env bash
# Rebuild the cmd/cmt archive. Installs the package list and
# copies all the cmt/cmti/cmd files found in the OPAM build
# dir into a single directory structure, separated by MD5
# to keep files distinct.

OPAM_DIR=opam
DATA_DIR=data
PACKAGES_FILE=packages

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

IMPORTS=`cat ${PACKAGES_FILE} | grep -v '^#' | awk '{print $1}'`

for imp in ${IMPORTS}; do 
  echo "Building $imp .."
  opam install -y -b --root=${OPAM_DIR} $imp
done

rm -rf ${DATA_DIR}
mkdir ${DATA_DIR}

PKGS=$(find ${OPAM_DIR}/system/build -mindepth 1 -maxdepth 1 -type d)

for pkg in ${PKGS}; do
  pkgname=$(basename $pkg)
  echo $pkgname
  mkdir ${DATA_DIR}/$pkgname
  CMDS=$(find ${pkg} -type f -name \*.cmd)
  for cmd in ${CMDS}; do
    d=$(dirname $cmd)
    calc_md5_for_file "$cmd";
    f=$(basename $cmd .cmd)
    mkdir ${DATA_DIR}/$pkgname/$md5
    r=${DATA_DIR}/$pkgname/$md5/$f
    cp $d/$f.cmd $r.cmd
  done
  CMDIS=$(find ${pkg} -type f -name \*.cmdi)
  for cmdi in ${CMDIS}; do
    d=$(dirname $cmdi)
    calc_md5_for_file "$cmdi";
    f=$(basename $cmdi .cmdi)
    mkdir ${DATA_DIR}/$pkgname/$md5
    r=${DATA_DIR}/$pkgname/$md5/$f
    cp $d/$f.cmdi $r.cmdi
  done
  CMTS=$(find ${pkg} -type f -name \*.cmt)
  for cmt in ${CMTS}; do
    d=$(dirname $cmt)
    calc_md5_for_file "$cmt";
    f=$(basename $cmt .cmt)
    mkdir ${DATA_DIR}/$pkgname/$md5
    r=${DATA_DIR}/$pkgname/$md5/$f
    cp $d/$f.cmt $r.cmt
  done
  CMTIS=$(find ${pkg} -type f -name \*.cmti)
  for cmti in ${CMTIS}; do
    d=$(dirname $cmti)
    calc_md5_for_file "$cmti";
    f=$(basename $cmti .cmti)
    mkdir ${DATA_DIR}/$pkgname/$md5
    r=${DATA_DIR}/$pkgname/$md5/$f
    cp $d/$f.cmti $r.cmti
  done
done
