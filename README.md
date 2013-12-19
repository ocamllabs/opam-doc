## opam-doc

Produce documentation for OPAM packages.

* Depending on : opam, cow, compiler-libs.common, unix, ocamlfind
* Requires: system compiler version >= 4.01

### Usage

Compile the library and and install it by running:

```
make
make install
```

This will create a new OPAM switch called `doc`, using a system
compiler switch with two small wrappers scripts which will call
`bin-doc` after each invocation of the compiler.

Now you can generate the documentation for any package using:

```
opam doc PKG1 ... PKGn
```

The packages will be installed in the `doc` switch, and their
documentation will be served on `http:127.0.0.1:8000`.