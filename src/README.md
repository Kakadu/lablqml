In this folder you can find code which performs code generation for Qt?+OCaml projects. It contains 1 library and 3 executables.
  * Library `lablqt` declares basic API and types for representing inforamtion about C++ classes. All executables depend on it.
  * Executable `generator` is used to generate binding code for QtGui+OCaml. Its implementation sucks but works somehow.
  * Executable `xmltool` is used to filter input XML files for `generator`.
  * Executable `mocml` is a generator for OCaml+QML project. You don't need to other executables for hacking QML-related ocaml code. `mocml` reads input file in JSON format, and using this information about C++ API generates wrappers to call OCaml from C++/ECMAscript. For some examples see `../qml` directory


