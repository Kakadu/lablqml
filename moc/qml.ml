open Core
open Parse
open Printf
open Helpers

open Parse.Yaml2
open Types

let ocaml_name_of_prop ~classname sort ({name;typ;_}) : string =
  sprintf "prop_%s_%s_%s_%s" classname name 
    (match sort with `Getter -> "get" | `Setter -> "set")
    (match typ  with `Simple s -> s | `List l -> sprintf "%s_list" l)

let gen_header {classname; members; slots; props; _ } =
  let h = open_out (classname ^ ".h") in
  let big_name = String.capitalize classname ^ "_H" in
  fprintf h "#ifndef %s\n" big_name;
  fprintf h "#define %s\n\n" big_name;
  fprintf h "#include <QObject>\n#include <QDebug>\n#include <kamlo.h>\n\n";
  fprintf h "class %s : public QObject {\n" classname;
  fprintf h "  Q_OBJECT\n";
  fprintf h "public:\n";
  (* properties *)
  List.iter props ~f:(fun ({name;getter;setter;notifier;typ} as prop) ->
    fprintf h "public:\n";
    fprintf h "  Q_PROPERTY(%s %s WRITE %s READ %s NOTIFY %s)\n" 
      (to_cpp_type typ) name setter getter notifier;
    let ocaml_methname (x,y,_) = ocaml_name_of_prop ~classname `Getter prop in
    Qtgui.gen_meth ~classname ~ocaml_methname ~invokable:false h (getter,[`Simple "unit"],typ);
    let ocaml_methname (x,y,_) = ocaml_name_of_prop ~classname `Setter prop in
    Qtgui.gen_meth ~classname ~ocaml_methname ~invokable:false h (setter,[typ],`Simple "unit");
    fprintf h "signals:\n";
    fprintf h "  void %s();\n" notifier
  );
  fprintf h "public:\n";
  fprintf h "  explicit %s(QObject *parent = 0) : QObject(parent) {}\n" classname;
  List.iter members ~f:(Qtgui.gen_meth ~classname ~invokable:true ~ocaml_methname:name_for_slot h);
  if slots <> [] then (
    fprintf h "public slots:\n";
    List.iter slots ~f:(Qtgui.gen_meth ~classname ~invokable:false ~ocaml_methname:name_for_slot h)
  );
  fprintf h "};\n";
  fprintf h "#endif\n\n";
  close_out h

let gen_cpp {classname; members; _ } =
  let h = open_out (classname ^ ".cpp") in
  fprintf h "#include \"%s.h\"\n" classname;

  close_out h
  










