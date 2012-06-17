open Core.Std
open Parse
open Printf
open Helpers

(* Generatess C++ stub for method *)
let gen_meth ~classname ~ocaml_methname ?(invokable=false) h ((name,args,res) as slot) = 
  let args = if args = [`Simple "unit"] then [] else args in
  let argnames = List.mapi args ~f: (fun i _ -> "x" ^ string_of_int i) in
  let lst = List.map args ~f:to_cpp_type in
  let arg' = List.map2_exn lst argnames ~f:(sprintf "%s %s") in
  let () = 
      fprintf h "  %s%s %s(%s) {\n" 
        (if invokable then "Q_INVOKABLE " else "") (to_cpp_type res)
        name (String.concat ~sep:"," arg') in
  fprintf h "    CAMLparam0;\n";
  let locals = 
    (match res with `Simple _ -> [] | `List _ -> ["head"]) 
    @ (if List.exists args ~f:(function `List _ -> true | `Simple _ -> false) 
      then ["list_cons_helper"] else [])
    @ ["_ans"]
  in
  fprintf h "    CAMLlocal%d(%s);\n" (List.length locals) (String.concat ~sep:"," locals);
  let ocaml_closure = ocaml_methname slot in
    (* TODO: use caml_callback, caml_callback2, caml_callback3 to speedup *)
    
    output_string h ("    value *closure = caml_named_value(\"" ^ ocaml_closure ^ "\");\n");
    fprintf h "    Q_ASSERT_X(closure!=NULL, \"%s::%s\",\n"            classname name;
    fprintf h "               \"ocaml's closure `%s` not found\");\n"   ocaml_closure;
    if List.exists args ~f:(function `List _ -> true | `Simple _ -> false) 
    then fprintf h "    value list_cons_helper = 0;\n";
    let n = List.length argnames in
    let call_closure_str = match n with 
      | 0 -> "caml_callback(*closure, Val_unit)"
      | _ -> begin
        fprintf h "    value *args = new value[%d];\n" n;
        List.iter2i args argnames ~f:(fun i arg name ->
          match arg with
            | `Simple "int"    -> fprintf h "    args[%d] = Val_int (%s);\n" i name
            | `Simple "bool"   -> fprintf h "    args[%d] = Val_bool(%s);\n" i name
            | `Simple "float"  -> failwithf "float values are not yet supported" ()
            | `Simple "unit"   -> fprintf h "    args[%d] = Val_unit;\n" i
            | `Simple "QString"
            | `Simple "string" -> 
                fprintf h "    args[%d] = caml_copy_string(%s.toLocal8Bit().data() );\n" i name
            | `Simple s when is_qt_classname s -> 
                fprintf h "    args[%d] = (%s!=0)? Some_val(%s) : Val_none;\n" i name name
            | `List "int" -> 
                fprintf h "    if (%s.length() == 0) args[%d] = Val_emptylist;\n" name i;
                fprintf h "    else {\n";
                fprintf h "      auto i = %s.end()-1;\n" name;
                fprintf h "      args[%d] = Val_emptylist;\n" i;
                fprintf h "      for (;;) {\n";
                fprintf h "        list_cons_helper = caml_alloc(2,0);\n";
                fprintf h "        Store_field(list_cons_helper, 0, Val_int(*i) );      //head\n";
                fprintf h "        Store_field(list_cons_helper, 1, list_cons_helper ); //tail\n";
                fprintf h "        ans[%d] = list_cons_helper;\n" i;
                fprintf h "        if (%s.begin() == i) break;\n" name;
                fprintf h "        i--;\n";
                fprintf h "      }\n";
                fprintf h "    }\n";
            | `List s -> failwithf "cannot convert parameter `List %s"  s ()
            | `Simple s -> failwithf "Cannot convert type %s" s ()
        );
        fprintf h "    // delete args or not?\n";
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
    if (res = `Simple "void" or res = `Simple "unit") then begin
      fprintf h "    %s;\n" call_closure_str;
      fprintf h "    CAMLreturn0;\n"
    end else begin
      match res with 
        | `Simple s ->
            let is_class = is_qt_classname s in
            fprintf h "    %s _ans = %s;\n"
              (if is_class then "value" else s) call_closure_str;
            fprintf h "    return %s;\n" (get_conv s)
        | `List s  -> 
            let conv = get_conv ~varname:"head" s in
            fprintf h "    QList<%s> ans;\n" s;
            fprintf h "    value head;\n";
            fprintf h "    while (_ans != Val_emptylist) {\n";
            fprintf h "       head = Field(_ans, 0);\n";
            fprintf h "       ans << %s;\n" conv;
            fprintf h "       _ans = Field(_ans, 1);\n";
            fprintf h "    };\n";
            fprintf h "    CAMLreturnT(QList<int>,ans);\n"            
    end;
    fprintf h "  }\n"

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
  List.iter lst ~f:(gen_meth ~classname ~ocaml_methname:name_for_slot h);
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

let gen_ml lst =
  let h = open_out (classname ^ "_stubs.ml") in
  fprintf h "open Stub_helpers\n";
  fprintf h "open Classes\n\n";

  List.iter lst ~f: (fun ((name, lst, typ) as slot) -> 
    
    let argnames = 
      List.mapi lst ~f: (fun i _ -> "x" ^ string_of_int i) in
    let stub_name = name_for_slot slot in
    fprintf h "let %s %s =\n" stub_name (String.concat ~sep:" " argnames);
    let args' = List.map2_exn argnames lst ~f: (fun argname typ -> match typ with
      | `Simple "int"
      | `Simple "bool" -> argname
      | `Simple "unit" -> sprintf " () "
      | `Simple s when is_qt_classname s ->
          sprintf "(match %s with Some x -> Some (new %s x) | None -> None)" argname s
      | _ -> failwithf "Can't cast type"  ()
    ) in
    fprintf h "  UserSlots.%s %s\n" name (String.concat ~sep:" " args');
    fprintf h "let () = Callback.register \"%s\" %s\n\n\n" stub_name stub_name
  );
  (* now create OCaml class *)
  fprintf h "class %s me = object \n" ocaml_classname;
  fprintf h "  method handler : [`qobject] obj = me\n";
  let to_ocaml_typ = function `Simple s -> s | `List s -> sprintf "(%s list)" s in
  List.iter lst ~f: (fun (name, lst', typ) ->
    let argnames = List.mapi lst' ~f:(fun i _ -> sprintf "arg%d" i) in
    fprintf h "  method slot_%s = object (self: (<%s..>, %s%s) #ssslot)\n" name
      (List.map2_exn argnames lst' 
         ~f:(fun name t -> sprintf "%s:%s; " name (to_ocaml_typ t)) |> String.concat)
      (lst' |> List.map ~f:to_ocaml_typ |> String.concat ~sep:"->") 
      (match typ with `Simple s -> s| `List s -> s ^ " list");
    fprintf h "    method name = \"%s(%s)\"\n" name
      (lst' |> List.map ~f:to_cpp_type |> String.concat ~sep:",")
    ;
    fprintf h "    method call = UserSlots.%s\n" name;
    fprintf h "  end\n"
  );
  fprintf h "end\n\n";

  fprintf h "external create_%s': unit -> [`qobject] obj = \"create%s\"\n" classname classname;
  fprintf h "let create_%s () = create_%s' () |> new %s\n" classname classname ocaml_classname;
  close_out h



