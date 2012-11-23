open Core
open Core.Std
open Parse
open Printf
open Helpers

open Parse.Yaml2
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
        let n = List.length args in (* because 1st is cpp object *)
        let argnames = List.init n ~f:(fun _ -> get_ocaml_var ()) in
        List.map2_exn argnames ( args) ~f:(fun argname arg_typ ->
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

  (*
(* How C method will be called *)
let get_stub_name ~classname (name,args,res) sort =
  sprintf "ml"%s_ *)

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
    let cpp_typ = TypAst.to_cpp_type typ in
    bprintf publics "  Q_PROPERTY(%s %s %s READ %s NOTIFY %s)\n" cpp_typ
      name (match setter with Some x -> "WRITE "^x | None -> "") getter notifier;
        
    let private_varname = sprintf "_%s" name in
    bprintf privates "  %s %s;\n" cpp_typ private_varname;
    bprintf publics "  %s %s() { return %s; }\n" cpp_typ getter private_varname;
    let () = match setter with
      | None -> ()
      | Some setter ->
          bprintf publics "  void %s(%s v) {\n" setter cpp_typ;
          bprintf publics "    if (%s != v) {\n" private_varname;
          bprintf publics "      %s = v;\n" private_varname;
          bprintf publics "      emit %s(v);\n" notifier;
          bprintf publics "  } }\n"
    in
    
    bprintf signals "  void %s(%s);\n" notifier (TypAst.to_cpp_type typ);
    gen_signal_stub ~classname ~signal:notifier ~typ cpp_buf (stubname_for_signal_emit name notifier);
    bprintf publics "  void emit_%s(%s arg1) {\n" notifier (TypAst.to_cpp_type typ);
    bprintf publics "    emit %s(arg1);\n" notifier;
    bprintf publics "  }\n\n";

    (* Stubs for calling setter and getter in OCaml *)
    gen_stub cpp_buf ~classname ~meth:(getter,[`Unit],typ) 
      (ocaml_name_of_prop ~classname `Getter prop);
    match setter with 
      | None -> ()
      | Some s -> 
          gen_stub cpp_buf ~classname ~meth:(s,[typ],`Unit) (ocaml_name_of_prop ~classname `Setter prop)
  );
  bprintf publics "public:\n";
  bprintf publics "  explicit %s(QObject *parent = 0) : QObject(parent) {}\n" classname;
  List.iter members ~f:(gen_meth ~classname ~options:[`Invokable]
                          ~ocaml_methname:name_for_slot publics cpp_buf);
  if slots <> [] then (
    List.iter slots ~f:(gen_meth ~classname ~options:[] ~ocaml_methname:name_for_slot slots_buf cpp_buf)
  );

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
  List.iter props ~f:(fun {typ; name; notifier; _} ->
    p "external emit_signal_%s: t -> %s -> unit = \"%s\"\n" name (TypAst.to_ocaml_type typ) 
      (stubname_for_signal_emit name notifier)
  );
  p "class %s cppval = object\n" (String.lowercase classname);
  List.iter props ~f:(fun {typ; name; notifier; _} ->
    p "  method emit_%s = emit_signal_%s cppval\n" name name
  );
  
  p "end\n";
  let ch = open_out ("ocaml/"^classname^".ml") in
  B.output_buffer ch h_buf;
  Out_channel.close ch



















