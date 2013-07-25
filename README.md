opam-doc
=========

Produce documentation for libraries installed with OPAM

Depending on : cow, compiler-libs.common, unix

Requires compiler version >= 4.01

==== 

Usage : 
- run bin-doc on your package's ml/mli files to get the cmd[i]s
- compile your package source files adding the '-bin-annot' option in order to generate the typed tree files (cmt[i]) 
- cd to a directory that will contain all the opam-doc generated documentation (e.g ~/.opam/4.X.X/doc)
- run : opam-doc --package 'package_name' \*.cmts \*cmds
