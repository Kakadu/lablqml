open Core
open Core.Std
open ParseYaml
open Printf
open Helpers

open ParseYaml.Yaml2
open Types

open Bigbuffer.Printf
include Qml

(* Generates stubs which calls method of specific class.
 * This stub will be called from
 *)
let gen_stub buf ~classname ~meth:(name,args,res_typ) caml_stub_name =
  let p fmt = bprintf buf fmt in
  let argnames = "_obj" :: (List.init (List.length args) ~f:(sprintf "_v%d")) in

  let locals_count = 1 + (* for _ans *)
    List.fold_left ~f:(fun acc x -> max acc (TypAst.aux_variables_count x)) ~init:0 (res_typ::args) in
  let locals = List.init locals_count ~f:(sprintf "_q%d") in
  let (get_ocaml_var,release_ocaml_var) = get_vars_queue locals in

  p "// %s\n" (List.map (args@[res_typ]) ~f:TypAst.to_ocaml_type |> String.concat ~sep:" -> ");
  p "extern \"C\" CAMLprim value %s(value %s) {\n" caml_stub_name (String.concat ~sep:", value " argnames);
  print_param_declarations buf argnames;
  print_local_declarations buf locals;
  p "  %s *obj = (%s*) (Field(_obj,0));\n" classname classname;
  let get_cpp_var = getter_of_cppvars "xx" in

  let calling_str =
    if args = [`Unit] then begin
      (* seems to be a getter*)
      sprintf "obj->%s()" name
    end else begin
      let param_names =
        List.map2_exn (List.tl_exn argnames) (args) ~f:(fun argname arg_typ ->
          let cpp_var = get_cpp_var () in
          p "  %s %s;\n" (TypAst.to_cpp_type arg_typ) cpp_var;
          cpp_value_of_ocaml buf (get_ocaml_var,release_ocaml_var,get_cpp_var)
            ~cpp_var ~ocaml_var:argname arg_typ;
          cpp_var
        )
      in
      sprintf "obj->%s(%s)" name (String.concat ~sep:", " param_names)
    end
  in

  let () = match res_typ with
    | `Unit ->
        p "  %s;\n" calling_str;
        p "  CAMLreturn(Val_unit);\n"
    | _ ->
        p "  %s ans = %s;\n" (TypAst.to_cpp_type res_typ) calling_str;
        let ansvar = get_ocaml_var () in
        ocaml_value_of_cpp buf ~tab:1 (get_ocaml_var,release_ocaml_var)
          ~cppvar:"ans" ~ocamlvar:ansvar res_typ;
        p "  CAMLreturn(%s);\n" ansvar;
        release_ocaml_var ansvar
  in
  p "}\n\n"

let setter_name_of_prop prop_name =
  let s = String.copy prop_name in
  s.[0] <- Char.uppercase s.[0];
  sprintf "set%s" s

(**
 * Generates C++ normal C++ class. Also generates stubs to for calling C++ from OCaml.
 *)
let gen_cpp {classname; members; slots; props; _ } =
  let big_name = String.capitalize classname ^ "_H" in
  let h_buf = B.create 100 in
  (* We will put in header file all methods bodies*)
  let print_h fmt = bprintf h_buf fmt in
  let cpp_buf  = B.create 100 in
  (* In C++ file we will put all *)
  let print_cpp fmt = bprintf cpp_buf fmt in

  print_time cpp_buf;
  print_time h_buf;
  print_h "#ifndef %s\n" big_name;
  print_h "#define %s\n\n" big_name;
  print_h "#include <QtCore/QObject>\n";
  print_h "#include <QtCore/QDebug>\n";
  print_h "#include \"kamlo.h\"\n";
  print_h "\n";
  print_h "class %s : public QObject\n{\n" classname;
  print_h "  Q_OBJECT\n";

  print_cpp "#include \"%s.h\"\n" classname;
  print_cpp "\n";

  (* properties *)
  let privates  = B.create 100 in
  let publics   = B.create 100 in
  let signals   = B.create 100 in
  let slots_buf = B.create 100 in

  List.iter props ~f:(fun ({name;getter;setter;notifier;typ} as prop) ->
    let (declare_setter,setter) = match setter with
      | Some s -> (true,s)
      | None   -> (false,"")
    in
    let cpp_typ = TypAst.to_cpp_type typ in
    bprintf publics "  Q_PROPERTY(%s %s%s READ %s NOTIFY %s)\n" cpp_typ
      name (if declare_setter then " WRITE "^setter else "") getter notifier;

    let private_varname = sprintf "_%s" name in
    bprintf privates "  %s %s;\n" cpp_typ private_varname;
    bprintf publics "  Q_INVOKABLE %s %s() { return %s; }\n" cpp_typ getter private_varname;
    let () =
      let setter = if declare_setter then setter else
          let s = String.copy name in
          s.[0] <- Char.uppercase s.[0];
          sprintf "set%s" s
      in
      bprintf publics "  %svoid %s(%s v) {\n" (if declare_setter then "Q_INVOKABLE " else "")
        setter cpp_typ;
      bprintf publics "    if (%s != v) {\n" private_varname;
      bprintf publics "      %s = v;\n" private_varname;(*
      bprintf publics "      qDebug() << \"%s changed\";\n" name;*)
      bprintf publics "      emit %s(v);\n" notifier;
      bprintf publics "  } }\n"
    in

    bprintf signals "  void %s(%s);\n" notifier (TypAst.to_cpp_type typ);
    gen_signal_stub ~classname ~signal:notifier ~typ cpp_buf (stubname_for_signal_emit name notifier);
    bprintf publics "  void emit_%s(%s arg1) {\n" notifier (TypAst.to_cpp_type typ);
    bprintf publics "    qDebug() << \"emitted %s\";\n" notifier;
    bprintf publics "    emit %s(arg1);\n" notifier;
    bprintf publics "  }\n\n";

    (* Stubs for calling setter and getter in OCaml *)
    gen_stub cpp_buf ~classname ~meth:(getter,[`Unit],typ)
      (ocaml_name_of_prop ~classname `Getter prop);
(*    if declare_setter then*)
    let setter_name = if declare_setter then setter else setter_name_of_prop name in
    gen_stub cpp_buf ~classname ~meth:(setter_name,[typ],`Unit) (ocaml_name_of_prop ~classname `Setter prop)
  );
  bprintf publics "public:\n";
  bprintf publics "  explicit %s(QObject *parent = 0) : QObject(parent) {}\n" classname;

  let ocaml_methname = name_for_slot ~ocaml_classname:classname in
  List.iter members ~f:(gen_meth ~classname ~options:[`Invokable]
                          ~ocaml_methname publics cpp_buf);
  List.iter slots ~f:(gen_meth ~classname ~options:[] ~ocaml_methname slots_buf cpp_buf);


  B.add_buffer h_buf privates;
  B.add_string h_buf "public:\n";
  B.add_buffer h_buf publics;
  B.add_string h_buf "public slots:\n";
  B.add_buffer h_buf slots_buf;
  B.add_string h_buf "signals:\n";
  B.add_buffer h_buf signals;


  print_h "};\n";
  print_h "#endif\n\n";

  let h_file = open_out (classname ^ ".h") in
  B.output_buffer h_file h_buf;
  Out_channel.close h_file;
  let cpp_file = open_out (classname ^ ".cpp") in
  B.output_buffer cpp_file cpp_buf;
  Out_channel.close cpp_file

let gen_ml {classname; members; slots; props; _ } =
  let h_buf = B.create 100 in
  print_time ~lang:`OCaml h_buf;
  (* we will put only functions for emitting signals *)
  let p fmt = bprintf h_buf fmt in
  p "type t\n";
  List.iter props ~f:(fun ({typ; name; notifier; setter; _} as prop) ->
    let ocaml_typ = TypAst.to_ocaml_type typ in
    p "external emit_signal_%s: t -> %s -> unit =\n  \"%s\"\n" name ocaml_typ
      (stubname_for_signal_emit name notifier);
    p "external prop_get_%s:    t -> unit -> %s =\n  \"%s\"\n" name ocaml_typ
      (ocaml_name_of_prop ~classname `Getter prop);
    p "external prop_set_%s:    t -> %s -> unit =\n  \"%s\"\n" name ocaml_typ
      (ocaml_name_of_prop ~classname `Setter prop)
  );
  p "\n";
  p "class %s cppval = object\n" (String.lowercase classname);
  List.iter props ~f:(fun {typ; name; notifier; setter; _} ->
    p "  method emit_%s = emit_signal_%s cppval\n" notifier name;
    p "  method %s = prop_get_%s cppval ()\n" name name;
    let setter_name = match setter with
      | None -> name
      | Some x -> x
    in
    p "  method set_%s = prop_set_%s cppval\n" setter_name name;
    p "\n"
  );

  p "end\n";
  let ch = open_out ("ocaml/"^classname^".ml") in
  B.output_buffer ch h_buf;
  Out_channel.close ch
