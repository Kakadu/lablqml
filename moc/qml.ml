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

let gen_cpp {classname; members; slots; props; _ } =
  let big_name = String.capitalize classname ^ "_H" in
  let h_file = open_out (classname ^ ".h") in
  let print_h fmt = fprintf h_file fmt in
  print_h "#ifndef %s\n" big_name;
  print_h "#define %s\n\n" big_name;
  print_h "#include <QtCore/QObject>\n";
  print_h "#include <QtCore/QDebug>\n";
  print_h "#include <kamlo.h>\n\n";
  print_h "class %s : public QObject\n{\n" classname;
  print_h "  Q_OBJECT\n";
  print_h "public:\n";

  let cpp_file = open_out (classname ^ ".cpp") in
  let print_cpp fmt = fprintf cpp_file fmt in
  print_cpp "#include \"%s.h\"\n" classname;

  (* properties *)
  List.iter props ~f:(fun ({name;getter;setter;notifier;typ} as prop) ->
    print_h "public:\n";
    print_h "  Q_PROPERTY(%s %s WRITE %s READ %s NOTIFY %s)\n" 
      (to_cpp_type typ) name setter getter notifier;
    let ocaml_methname (x,y,_) = ocaml_name_of_prop ~classname `Getter prop in
    Qtgui.gen_meth ~classname ~ocaml_methname ~invokable:false h_file cpp_file (getter,[`Simple "unit"],typ);
    let ocaml_methname (x,y,_) = ocaml_name_of_prop ~classname `Setter prop in
    Qtgui.gen_meth ~classname ~ocaml_methname ~invokable:false h_file cpp_file (setter,[typ],`Simple "unit");
    print_h "signals:\n";
    print_h "  void %s();\n" notifier
  );
  print_h "public:\n";
  print_h "  explicit %s(QObject *parent = 0) : QObject(parent) {}\n" classname;
  List.iter members ~f:(Qtgui.gen_meth ~classname ~invokable:true 
                          ~ocaml_methname:name_for_slot h_file cpp_file);
  if slots <> [] then (
    print_h "public slots:\n";
    List.iter slots ~f:(Qtgui.gen_meth ~classname ~invokable:false ~ocaml_methname:name_for_slot h_file cpp_file)
  );
  print_h "};\n";
  print_h "#endif\n\n";
  Out_channel.close h_file;
  Out_channel.close cpp_file
