open Core
open Parse
open Printf

module List = struct 
  include Core_list
  let iter2i l1 l2 ~f =
    let i = ref 0 in
    Core_list.iter2_exn l1 l2 ~f: (fun x y -> f !i x y; incr i)
end
module String = Core_string
module S = Std_internal

type options = {
  mutable filename:string
}

let options = {filename = "input"}

let () = Core_arg.parse [
  ] (fun s -> options.filename <- s; 
    print_endline ("Setting filename " ^ s)
) "usage_msg"

and () =
  let data = Parse.parse2 options.filename in
  print_endline "data file parsed";
  let ans = data |> sexp_of_api_content |> Sexplib.Sexp.to_string_hum in
  print_endline ans;
  exit 0

let funcs = Parse.parse options.filename
let () = print_endline ("slots parsed: " ^ string_of_int (List.length funcs))

let classname = "UserSlots"
let ocaml_classname = 
  let s = String.copy classname in
  s.[0] <- Core_char.lowercase s.[0];
  s

let to_cpp_type = function
  | "float" -> "double"
  | "unit"  -> "void"
  | "int"
  | "bool" as t -> t
  | s when s.[0] = 'q' -> let s = String.copy s in s.[0]<-'Q' ; s ^ "*"
  | _ -> assert false

let is_qt_classname s = (s.[0]='q') || (s.[0]='Q')

let to_qt_classname s = 
  if is_qt_classname s then (
    let s' = String.copy s in
    s'.[0] <- 'Q';
    s'
  ) else 
    Std_internal.failwithf "Bad arguments of to_qt_classname" ()

let name_for_slot (name,lst) = 
  let f = String.map ~f:(function '*' -> '_' | s -> s) in
  String.concat ~sep:"_" (ocaml_classname::name::(List.map lst ~f))

let gen_header lst = 
  let h = open_out (classname ^ ".h") in

  List.iter [
    "#include <Qt/QtOpenGL>\n";
    "extern \"C\" {\n";
    "#include \"headers.h\"\n";
    "}\n";
    "#include <QtCore/QObject>\n";
    "class" ^ classname ^ ": public QObject {\nQ_OBJECT\n";
    "public slots:\n";
  ] (output_string h);

  List.iter lst ~f: (fun ((name, lst) as slot) -> 
    let lst = List.map lst ~f: to_cpp_type in
    let (res, args) = 
      let l'= List.rev lst in
      (l' |> List.hd_exn, l' |> List.tl_exn |> List.rev)
    in
    let args = if args = ["void"] then [] else args in
    let argnames = List.mapi args ~f: (fun i _ -> "x" ^ string_of_int i) in
    let arg' = List.map2_exn args argnames ~f: (fun typ name -> sprintf "%s %s" typ name) in
    fprintf h "  %s %s(%s) {\n" res name (String.concat ~sep: "," arg');
    (* TODO: use caml_callback, caml_callback2, caml_callback3 to speedup *)
    output_string h ("    value *closure = caml_named_value(\"" ^ (name_for_slot slot) ^ "\");\n");
    output_string h  "    if (closure==NULL)\n      printf(\"closure not found. crash.\\n\");\n";

    let n = List.length argnames in
    let call_closure_str = match n with 
      | 0 -> "caml_callback(*closure, Val_unit)"
      | _ -> begin
        fprintf h "    value *args = new value[%d];\n" n;
        List.iter2i args argnames ~f:(fun i arg name ->
          match arg with
            | "int"    -> fprintf h   "    args[%d] = Val_int (%s);\n" i name
            | "bool"   -> fprintf h   "    args[%d] = Val_bool(%s);\n" i name
            | "float"  -> S.failwithf "float values are not yet supported" ()
            | "string" -> fprintf h   "    args[%d] = caml_copy_string(%s.toLocal8Bit().data() );\n" i name
            | _ when is_qt_classname arg -> begin
(*              let classname = to_qt_classname arg in *)
              fprintf h "    args[%d] = (%s!=0)? Some_val(%s) : Val_none;\n" i name name
            end
            | _ -> assert false
        );
        fprintf h "    // delete args or not?\n";
        sprintf "caml_callbackN(*closure, %d, args)" n
      end
    in
    if res = "void" then
      fprintf h "    %s;\n" call_closure_str
    else begin
      let is_class = is_qt_classname res in
      fprintf h "    %s _ans = %s;\n"
        (if is_class then "value" else res) call_closure_str;
      let ret = match res with
        | "int"   -> "Int_val(_ans)"
        | "double"
        | "float" -> assert false
        | "bool"  -> "Bool_val(_ans)"
        | _ when
          is_class  -> sprintf " (%s)_ans" res
        | "string"
        | "QString" -> "QString(String_val(_ans))"
        | _ -> assert false
      in
      fprintf h "    return %s;\n" ret
    end;
    fprintf h "  }\n"
  );
  fprintf h "};\n";
  close_out h;

  let h = open_out (classname ^ ".cpp") in
  fprintf h "#include \"%s.h\"\n" classname;
  fprintf h "extern \"C\" {\n";
  fprintf h "  #include \"headers.h\"\n";
  fprintf h "  value createUserSlots(value x) {\n";
  fprintf h "    CAMLparam1(x);\n";
  fprintf h "    CAMLreturn((value)(new %s()));\n" classname;
  fprintf h "  }\n";
  fprintf h "}\n\n";
  close_out h

let to_cpp_type s = match s with
  | "int"  -> "int"
  | "bool" -> "bool"
  | "unit" -> "void"
  | s when is_qt_classname s -> to_qt_classname s ^ "*"
  | _ -> assert false

let gen_ml lst =
  let h = open_out (classname ^ "_stubs.ml") in
  fprintf h "open Stub_helpers\n";
  fprintf h "open Classes\n\n";

  List.iter lst ~f: (fun ((name, lst) as slot) -> 
    let lst = List.rev lst |> List.tl_exn |> List.rev in
    let argnames = if lst = ["unit"] then ["()"] 
      else List.mapi lst ~f: (fun i _ -> "x" ^ string_of_int i) in
    let stub_name = name_for_slot slot in
    fprintf h "let %s %s =\n" stub_name (String.concat ~sep:" " argnames);
    let args' = List.map2_exn argnames lst ~f: (fun argname typ -> match typ with
      | "int"
      | "bool" -> argname
      | "unit" -> sprintf " () "
      | s when is_qt_classname s ->
        sprintf "(match %s with Some x -> Some (new %s x) | None -> None)" argname s
      | _ -> S.failwithf "Can't cast type: %s" typ ()
    ) in
    fprintf h "  UserSlots.%s %s\n" name (String.concat ~sep:" " args');
    fprintf h "let () = Callback.register \"%s\" %s\n\n\n" stub_name stub_name
  );
  (* now create OCaml class *)
  fprintf h "class %s me = object \n" ocaml_classname;
  fprintf h "  method handler : [`qobject] obj = me\n";
  List.iter lst ~f: (fun (name, lst) ->
    let lst' = List.rev lst |> List.tl_exn |> List.rev in
  (*let res = List.rev lst |> List.hd_exn in*)
    let argnames = List.mapi lst' ~f: (fun i _ -> sprintf "arg%d" i) in
    fprintf h "  method slot_%s = object (self: (<%s..>, %s) #ssslot)\n" name
      (List.map2_exn argnames lst' ~f: (fun name typ -> sprintf "%s:%s; " name typ) |> String.concat)
      (String.concat ~sep:"->" lst);
    fprintf h "    method name = \"%s(%s)\"\n" name
      (List.map ~f: to_cpp_type lst' |> String.concat ~sep:",")
    ;
    fprintf h "    method call = UserSlots.%s\n" name;
    fprintf h "  end\n"
  );
  fprintf h "end\n\n";

  fprintf h "external create_%s': unit -> [`qobject] obj = \"create%s\"\n" classname classname;
  fprintf h "let create_%s () = create_%s' () |> new %s\n" classname classname ocaml_classname;
  close_out h


let () = gen_header funcs
let () = gen_ml funcs
