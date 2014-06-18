open Core_kernel.Std
open ParseYaml
open Printf
open Helpers

(* Generatess C++ stub for method *)
let gen_meth ~classname ~ocaml_methname ?(invokable=false)
    file_h file_cpp ((name,args,res) as slot) =
  printf "Generatig meth '%s'\n" name;
  let print_h   fmt = fprintf file_h   fmt in
  let print_cpp fmt = fprintf file_cpp fmt in
  print_h "  //Generating meth '%s'\n" name;
  let args = if args = [`Simple "unit"] then [] else args in
  let argnames = List.mapi args ~f: (fun i _ -> "x" ^ string_of_int i) in
  let lst = List.map args ~f:to_cpp_type in
  let arg' = List.map2_exn lst argnames ~f:(sprintf "%s %s") in
  let () =
    print_h  "  %s%s %s(%s);\n"
    (if invokable then "Q_INVOKABLE " else "") (to_cpp_type res)
    name (String.concat ~sep:"," arg') in
  let () =
    print_cpp "%s %s::%s(%s) {\n" (to_cpp_type res) classname
    name (String.concat ~sep:"," arg') in
  print_cpp "  CAMLparam0();\n";
  let locals =
    (match res with `Simple _ -> [] | `List _ -> ["head"])
    @ (if List.exists args ~f:(function `List _ -> true | `Simple _ -> false)
      then ["list_cons_helper"] else [])
    @ ["_ans"]
  in
  print_cpp "  CAMLlocal%d(%s);\n" (List.length locals) (String.concat ~sep:"," locals);
  let ocaml_closure = ocaml_methname slot in
  (* TODO: use caml_callback, caml_callback2, caml_callback3 to speedup *)
  print_cpp "  value *closure = caml_named_value(\"%s\");\n" ocaml_closure;
  print_cpp "  Q_ASSERT_X(closure!=NULL, \"%s::%s\",\n"      classname name;
  print_cpp "             \"ocaml's closure `%s` not found\");\n"   ocaml_closure;
  if List.exists args ~f:(function `List _ -> true | `Simple _ -> false)
  then print_cpp "  value list_cons_helper = 0;\n";
  let n = List.length argnames in
  let call_closure_str = match n with
      | 0 -> "caml_callback(*closure, Val_unit)"
      | _ -> begin
        print_cpp "  value *args = new value[%d];\n" n;
        List.iter2i args argnames ~f:(fun i arg name ->
          match arg with
            | `Simple "int"    -> print_cpp "  args[%d] = Val_int (%s);\n" i name
            | `Simple "bool"   -> print_cpp "  args[%d] = Val_bool(%s);\n" i name
            | `Simple "float"  -> failwithf "float values are not yet supported" ()
            | `Simple "unit"   -> print_cpp "  args[%d] = Val_unit;\n" i
            | `Simple "QString"
            | `Simple "string" ->
                print_cpp "  args[%d] = caml_copy_string(%s.toLocal8Bit().data() );\n" i name
            | `Simple s when is_qt_classname s ->
                print_cpp "  args[%d] = (%s!=0)? Some_val(%s) : Val_none;\n" i name name
            | `List "int" ->
                print_cpp "  if (%s.length() == 0) args[%d] = Val_emptylist;\n" name i;
                print_cpp "  else {\n";
                print_cpp "    auto i = %s.end()-1;\n" name;
                print_cpp "    args[%d] = Val_emptylist;\n" i;
                print_cpp "    for (;;) {\n";
                print_cpp "        list_cons_helper = caml_alloc(2,0);\n";
                print_cpp "        Store_field(list_cons_helper, 0, Val_int(*i) );      //head\n";
                print_cpp "        Store_field(list_cons_helper, 1, list_cons_helper ); //tail\n";
                print_cpp "        ans[%d] = list_cons_helper;\n" i;
                print_cpp "        if (%s.begin() == i) break;\n" name;
                print_cpp "        i--;\n";
                print_cpp "    }\n";
                print_cpp "  }\n";
            | `List s -> failwithf "cannot convert parameter `List %s"  s ()
            | `Simple s -> failwithf "Cannot convert type %s" s ()
        );
        print_cpp "  // delete args or not?\n";
        sprintf "caml_callbackN(*closure, %d, args)" n
      end
    in
    let get_conv ?(varname="_ans") = function
      | "int"   -> sprintf "Int_val(%s)" varname
      | "double"
      | "float" -> assert false
      | "bool"  -> sprintf "Bool_val(%s)" varname
      | "string"
      | "QString" -> sprintf "QString(String_val(%s))" varname
      | c when is_qt_classname c -> sprintf " (%s)%s" c varname
      | s -> failwithf "Can't create cast of `%s`" s ()
    in
    if (res = `Simple "void" || res = `Simple "unit") then begin
      print_cpp "  %s;\n" call_closure_str;
      print_cpp "  CAMLreturn0;\n"
    end else begin
      match res with
        | `Simple s ->
            print_cpp "  _ans = %s;\n" call_closure_str;
            print_cpp "  return %s;\n" (get_conv s)
        | `List s  ->
            let conv = get_conv ~varname:"head" s in
            print_cpp "    QList<%s> ans;\n" s;
            print_cpp "    value head;\n";
            print_cpp "    while (_ans != Val_emptylist) {\n";
            print_cpp "       head = Field(_ans, 0);\n";
            print_cpp "       ans << %s;\n" conv;
            print_cpp "       _ans = Field(_ans, 1);\n";
            print_cpp "    };\n";
            print_cpp "    CAMLreturnT(QList<int>,ans);\n"
    end;
    print_cpp "}\n";
    flush file_cpp


let gen_cpp lst =
  let h_file = open_out (classname ^ ".h") in
  List.iter [
    "#include <Qt/QtGui>\n";
    "extern \"C\" {\n";
    "#include \"headers.h\"\n";
    "}\n";
    "#include <QtCore/QObject>\n\n";
    "class " ^ classname ^ ": public QObject {\nQ_OBJECT\n";
    "public slots:\n";
  ] ~f:(output_string h_file);
  let cpp_file = open_out (classname ^ ".cpp") in
  fprintf cpp_file "#include \"%s.h\"\n\n" classname;
  List.iter lst ~f:(gen_meth ~classname ~ocaml_methname:name_for_slot h_file cpp_file);
  output_string h_file  "\n};\n";
  Out_channel.close h_file;
  Out_channel.close cpp_file;

  let cpp_file = open_out (classname ^ "_helper.cpp") in
  fprintf cpp_file "#include \"%s.h\"\n" classname;
  fprintf cpp_file "extern \"C\" {\n";
  fprintf cpp_file "  #include \"headers.h\"\n";
  fprintf cpp_file "  value createUserSlots(value x) {\n";
  fprintf cpp_file "    CAMLparam1(x);\n";
  fprintf cpp_file "    CAMLreturn((value)(new %s()));\n" classname;
  fprintf cpp_file "  }\n";
  fprintf cpp_file "}\n\n";
  Out_channel.close cpp_file


let gen_ml lst =
  let h = open_out (classname ^ "_stubs.ml") in
  let print_cpp  fmt = fprintf h fmt in
  print_cpp "open Stub_helpers\n";
  print_cpp "open Classes\n\n";

  List.iter lst ~f: (fun ((name, lst, typ) as slot) ->

    let argnames =
      List.mapi lst ~f: (fun i _ -> "x" ^ string_of_int i) in
    let stub_name = name_for_slot slot in
    print_cpp "let %s %s =\n" stub_name (String.concat ~sep:" " argnames);
    let args' = List.map2_exn argnames lst ~f: (fun argname typ -> match typ with
      | `Simple "int"
      | `Simple "bool" -> argname
      | `Simple "unit" -> sprintf " () "
      | `Simple s when is_qt_classname s ->
          sprintf "(match %s with Some x -> Some (new %s x) | None -> None)" argname s
      | _ -> failwithf "Can't cast type"  ()
    ) in
    print_cpp "  UserSlots.%s %s\n" name (String.concat ~sep:" " args');
    print_cpp "let () = Callback.register \"%s\" %s\n\n\n" stub_name stub_name
  );
  (* now create OCaml class *)
  print_cpp "class %s me = object \n" ocaml_classname;
  print_cpp "  method handler : [`qobject] obj = me\n";
  let to_ocaml_typ = function `Simple s -> s | `List s -> sprintf "(%s list)" s in
  List.iter lst ~f: (fun (name, lst', typ) ->
    let argnames = List.mapi lst' ~f:(fun i _ -> sprintf "arg%d" i) in
    print_cpp "  method slot_%s = object (self: (<%s..>, %s%s) #ssslot)\n" name
      (List.map2_exn argnames lst'
         ~f:(fun name t -> sprintf "%s:%s; " name (to_ocaml_typ t)) |> String.concat)
      (lst' |> List.map ~f:to_ocaml_typ |> String.concat ~sep:"->")
      (match typ with `Simple s -> s| `List s -> s ^ " list");
    print_cpp "    method name = \"%s(%s)\"\n" name
      (lst' |> List.map ~f:to_cpp_type |> String.concat ~sep:",")
    ;
    print_cpp "    method call = UserSlots.%s\n" name;
    print_cpp "  end\n"
  );
  print_cpp "end\n\n";

  print_cpp "external create_%s': unit -> [`qobject] obj = \"create%s\"\n" classname classname;
  print_cpp "let create_%s () = create_%s' () |> new %s\n" classname classname ocaml_classname;
  Out_channel.close h
