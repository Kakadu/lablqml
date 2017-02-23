`lablqml` is a library for interfacing OCaml and QtQuick. 

Run `make` to build and `make tests` to run tests.

Edit `QmlContext.ml[i]` to adjust OCaml API. 

Add C++ source files and headers to the stubs directory. Write them down in the
`liblablqml_stubs.clib` to put these C++ objects into final `lablqml.cm[x]a`. 
If you add some headers in the stubs directory specify them as dependencies in 
the  `myocamlbuild.ml` to make them being copied to the `_build` directory. For 
instance:

    dep ["compile"; "c"] ["stubs/lablqml.h"];
    dep ["compile"; "qsinglefunc"] ["stubs/QSingleFunc.h"];
    dep ["compile"; "camlpropmap"] ["stubs/CamlPropertyMap.h"];

There are some test demos in the `src_tests` directory. We can run them using
`make tests`.


