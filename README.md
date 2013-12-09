opam-doc
=========

Produce documentation for OPAM packages.

Depending on : opam, cow, compiler-libs.common, unix, ocamlfind

Requires compiler version >= 4.01

====

Usage :

1. Write the names of the packages for which you want to generate documentation into the `packages` file.

2. Write the URL from which you intend to serve the documentation into the `url` file.

3. Write any additional OPAM repositories you need into the `repositories` file.

4. Run `make`.

5. The documentation should now be in the `doc` folder.