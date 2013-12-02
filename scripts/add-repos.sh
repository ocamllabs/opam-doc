#!/usr/bin/env bash

OPAM_DIR=opam
REPOS_FILE=repositories

REPOS=`awk '{print $1}' ${REPOS_FILE}`

count=0
for repo in ${REPOS}; do 
  let count=count+1 
  echo "Adding $repo as repo$count .."
  opam remote add repo$count --root=${OPAM_DIR} $repo
done