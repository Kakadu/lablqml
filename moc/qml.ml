open Core
open Parse
open Printf
open Helpers

open Parse.Yaml2
open Types

let gen_header {classname; members; _ } =
  let h = open_out (classname ^ ".h") in
  let big_name = String.capitalize classname ^ "_H" in
  fprintf h "#ifndef %s\n" big_name;
  fprintf h "#define %s\n\n" big_name;
  fprintf h "#include <QObject>\n#include <QDebug>\n#include <kamlo.h>\n";
  fprintf h "class %s : public QObject {\n" classname;
  fprintf h "  Q_OBJECT\n";
  fprintf h "public:\n";
  fprintf h "  explicit %s(QObject *parent = 0) : QObject(parent) {};n" classname;

  fprintf h "signals:\n";
  fprintf h "public slots:\n";
  fprintf h "};\n";
  fprintf h "#endif\n\n";
  close_out h

let gen_cpp {classname; members; _ } =
  let h = open_out (classname ^ ".cpp") in
  fprintf h "#include \"%s.h\"\n" classname;

  close_out h
  










