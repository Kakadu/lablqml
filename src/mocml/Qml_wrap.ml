open Core.Std
module B=Bigbuffer
open Helpers
open B.Printf
open ParseYaml.Yaml2.Types


let qabstractItemView_members =
      let open Parser in
      let unref_model = unreference qmodelindex_type in
      let model = {qmodelindex_type with t_is_const=true} in
        [ ("parent",      [model],  unref_model, [`Const])
        ; ("index",       [int_type; int_type; model], unref_model, [`Const])
        ; ("columnCount", [model], int_type, [`Const])
        ; ("rowCount",    [model], int_type, [`Const])
        ; ("hasChildren", [model], bool_type, [`Const])
        ; ("data",        [model; int_type], qvariant_type, [`Const])

        ]

let generate ?(directory=".") {classname; basename; members; slots; props; _} =
  let b_h   = B.create 100 in
  let b_c   = B.create 100 in
  let b_ml  = B.create 100 in
  Qml.print_time ~lang:`CPP b_h;
  Qml.print_time ~lang:`OCaml b_ml;
  (* we will put only functions for emitting signals *)
  let p_h   fmt = bprintf b_h fmt in
  let p_c   fmt = bprintf b_c fmt in
  let p_ml  fmt = bprintf b_ml fmt in


  p_h "#ifndef %s_c_H\n" classname;
  p_h "#define %s_c_H\n" classname;
  p_c "#include \"%s_c.h\"\n\n" classname;

  p_h "#include \"kamlo.h\"\n";
  p_h "#include <QtCore/QDebug>\n";
  p_h "#include <QtCore/QObject>\n";
  let base_classname =
    match basename with
      | None -> "QObject"
      | Some x ->
          p_h "#include <QtCore/%s>\n" x;
          x
  in
  p_h "\n";
  p_h "class %s: public %s {\n" classname base_classname;
  p_h "  Q_OBJECT\n";
  p_h "  value camlobj = 0;\n";
  p_h "public:\n";
  (* constructor *)
  p_h "  %s(value _camlobj);\n" classname;
  p_c "%s::%s(value _camlobj) : camlobj(_camlobj) {\n" classname classname;
  p_c "}\n";

  (* methods *)
  let do_meth ~classname (name,args,res,modif) = (*
    let args = List.map args TypAst.of_verbose_typ_exn in
    let res = TypAst.of_verbose_typ_exn res in *)
    let (_:Parser.cpptype list) = args in
    let (_:Parser.cpptype) = res in
    let args = if args = [Parser.void_type] then [] else args in
    p_h "  Q_INVOKABLE %s %s(%s)%s;\n" (Parser.string_of_type res) name
      (List.map args ~f:Parser.string_of_type |> String.concat ~sep:",")
      (if List.mem modif `Const then " const" else "");
    (* now source *)
    let locals_count = 1 + (* for _ans *)
      List.fold_left ~init:0 (res::args)
      ~f:(fun acc x -> max acc TypAst.(x |> of_verbose_typ_exn |> aux_variables_count))
    in
    let argnames_cpp = List.init ~f:(fun n -> sprintf "x%d" n) (List.length args) in
    p_c "%s %s::%s(%s) %s{\n" (Parser.string_of_type res) classname name
      (let types = List.map args ~f:Parser.string_of_type in
       List.map2_exn ~f:(sprintf "%s %s") types argnames_cpp |> String.concat ~sep:",")
      (if List.mem modif `Const then " const" else "");
    let locals = List.init ~f:(fun n -> sprintf "_x%d" n) (locals_count-1) in
    Qml.print_local_declarations b_c (["_ans"; "_meth"] @ locals);

    p_c "  qDebug() << \"Calling %s::%s\";\n" classname name;

    p_c "  _meth = caml_get_public_method(camlobj, caml_hash_variant(\"%s\"));\n" name;
    let (get_var, release_var) = Qml.get_vars_queue locals in (* tail because we need not _ans *)

    let call_closure_str = match List.length args with
      | 0 ->
          sprintf "caml_callback2(_meth, camlobj, Val_unit)"

      | n -> begin
        (* Generating arguments for calling *)
        p_c "  value *args = new value[%d];\n" (n+1);
        p_c "  args[0] = camlobj;\n"; (* Never forget that !!! *)
        List.iter2i args argnames_cpp ~f:(fun i arg cppvar ->
          Qml.ocaml_value_of_cpp b_c (get_var,release_var)
            ~tab:1 ~ocamlvar:(sprintf "args[%d]" (i+1) ) ~cppvar ( arg)
        );
        p_c "  // delete args or not?\n";
        sprintf "caml_callbackN(_meth, %d, args)" (n+1)
      end
    in
    if res = Parser.void_type then p_c " %s;\n" call_closure_str
    else begin
      p_c "  _ans = %s;\n" call_closure_str;
      let cpp_ans_var = "cppans" in

      let new_cpp_var = Qml.getter_of_cppvars "xx" in
      p_c "  %s %s;\n" (Parser.string_of_type res) cpp_ans_var;
      Qml.cpp_value_of_ocaml b_c (get_var,release_var, new_cpp_var) cpp_ans_var "_ans"
        ( res);
      p_c "  return %s;\n" cpp_ans_var;
    end;
    p_c "}\n";
  in
  List.iter members ~f:(do_meth ~classname);
  (* *)
  (* Now we will add some methods for specific basename *)
  let () =
    if base_classname = "QAbstractItemModel" then begin
      let model_members = qabstractItemView_members in
      List.iter model_members ~f:(do_meth ~classname);
      p_h "private:\n";
      p_h "  QHash<int, QByteArray> _roles;\n";
      p_h "public:\n";

      p_h "Q_INVOKABLE QList<QString> roles() {\n";
      p_h "  QList<QString> ans;\n";
      p_h "  foreach(QByteArray b, _roles.values() )\n";
      p_h "      ans << QString(b);\n";
      p_h "  return ans;\n";
      p_h "}\n";
      p_h "void addRole(int r,QByteArray name) { _roles.insert(r,name); }\n";
      p_h "virtual QHash<int, QByteArray> roleNames() const { return _roles; }\n";

      (* stub for adding new roles *)
      p_c "extern \"C\" value caml_%s_addRole(value cppobj,value num,value roleName) {\n" classname;
      p_c "  CAMLparam3(cppobj,roleName,num);\n";
      p_c "  %s *o  = (%s*) Field(cppobj,0);\n" classname classname;
      p_c "  o->addRole( Int_val(num), QByteArray(String_val(roleName)) );\n";
      p_c "  CAMLreturn(Val_unit);\n";
      p_c "}\n";

    end
  in

  (* Also we need to have a stubs to create C++ class *)
  p_c "extern \"C\" value caml_create_%s(value camlObj) {\n" classname;
  p_c "  CAMLparam1(camlObj);\n";
  p_c "  CAMLlocal1(_ans);\n";
  p_c "  _ans = caml_alloc_small(1, Abstract_tag);\n";
  p_c "  (*((%s **) &Field(_ans, 0))) = new %s(camlObj);\n" classname classname;
  p_c "  CAMLreturn(_ans);\n";
  p_c "}\n";

  p_h "};\n";
  p_h "#endif // %s_H\n" classname;

  p_ml "\n";
  (* Allow to create C++ class from OCaml *)
  p_ml "open QmlContext\n\n";
  p_ml "class type typ_for_%s = object\n" classname;

  let do_meth_caml = fun (name,args,res,_) ->
    let caml_types = List.map args ~f:(fun x -> x |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type) in
    p_ml "  method %s: %s %s\n" name
      (if args=[] then "" else (String.concat ~sep:"->" caml_types)^"->")
      (res |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
  in
  List.iter members ~f:do_meth_caml;
  let () =
    if base_classname = "QAbstractItemModel" then
      List.iter qabstractItemView_members ~f:(do_meth_caml);
  in
  p_ml "end\n";
  p_ml "external create_%s: typ_for_%s -> 'a = \"caml_create_%s\"\n" classname classname classname;
  if base_classname = "QAbstractItemModel" then begin
    p_ml "external add_role: 'a -> int -> string -> unit = \"caml_%s_addRole\"\n" classname
  end;
  let with_file path f =
    let file = open_out path in
    f file;
    Out_channel.close file
  in
  with_file (directory ^/ classname ^ "_c.cpp") (fun file -> B.output_buffer file b_c);
  with_file (directory ^/ classname ^ "_c.h")   (fun file -> B.output_buffer file b_h);
  with_file (directory ^/ classname ^ ".ml")  (fun file -> B.output_buffer file b_ml)
