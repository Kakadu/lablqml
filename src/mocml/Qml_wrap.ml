open Core
open Core.Std
module B=Bigbuffer
open Helpers
open B.Printf
open ParseYaml.Yaml2.Types
open Parser
open Qml

let with_file path f =
  let file = open_out path in
  f file;
  Out_channel.close file

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

(** generated C++ method which will be called from OCaml side
 *  returns c++ stub name *)
let gen_cppmeth_wrapper ~classname cbuf meth =
  let p_c fmt = bprintf cbuf fmt in
  let (name,args,res,_) = meth in
  let cpp_stub_name = sprintf "caml_%s_%s_cppmeth_wrapper" classname name in
  let argnames = "_cppobj" :: (List.mapi args ~f:(fun i _ -> sprintf "_x%d" i)) in
  p_c "extern \"C\" value %s(%s) {\n" cpp_stub_name
    (List.map argnames ~f:(sprintf "value %s")|> String.concat ~sep:",");
  Qml.print_param_declarations cbuf argnames;
  let args = if args=[void_type] then [] else args in
  let locals_count = 1 +
    List.fold_left ~init:0 (res::args)
      ~f:(fun acc x -> max acc TypAst.(x |> of_verbose_typ_exn |> aux_variables_count))
  in
  let locals = List.init ~f:(fun n -> sprintf "_z%d" n) locals_count in
  Qml.print_local_declarations cbuf locals;
  p_c "  %s *o = (%s*) (Field(_cppobj,0));\n" classname classname;
  let (get_var, release_var) = Qml.get_vars_queue locals in
  let new_cpp_var =
    let count = ref 0 in
    fun () -> incr count; sprintf "x%d" !count
  in
  let cpp_var_names = ref [] in

  List.iteri args ~f:(fun i typ ->
    let cpp_var = sprintf "z%d" i in
    Ref.replace cpp_var_names (fun xs -> cpp_var::xs);
    p_c "  %s %s;\n" (typ |> unreference |> string_of_type) cpp_var;
    let ocaml_var = List.nth_exn argnames (i+1) in
    cpp_value_of_ocaml ~options:[`AbstractItemModel (Some "o")] cbuf
      (get_var,release_var,new_cpp_var) ~cpp_var ~ocaml_var typ
  );
  let cpp_var_names = List.rev !cpp_var_names in
  let call_str = sprintf "  o->%s(%s);" name (String.concat ~sep:"," cpp_var_names) in
  p_c "  qDebug() << \"Going to call %s::%s\";\n" classname name;
  if res=void_type then begin
    p_c "%s\n" call_str;
    p_c "  CAMLreturn(Val_unit);\n"
  end else begin
    let cppvar = new_cpp_var () in
    p_c "  %s %s = %s\n" (res |> unreference |> string_of_type) cppvar call_str;
    let ocamlvar = get_var () in
    ocaml_value_of_cpp cbuf (get_var,release_var) ~tab:1 ~ocamlvar ~cppvar res;
    p_c "  CAMLreturn(%s);\n" ocamlvar
  end;
  p_c "}\n";
  cpp_stub_name

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

  p_ml "\nopen QmlContext\n\n";

  p_h "#ifndef %s_c_H\n" classname;
  p_h "#define %s_c_H\n" classname;
  p_c "#include \"%s_c.h\"\n\n" classname;

  p_h "#include \"kamlo.h\"\n";
  p_h "#include <QtCore/QDebug>\n";
  p_h "#include <QtCore/QObject>\n";
  let base_classname =
    match basename with
      | Some ""
      | None -> "QObject"
      | Some x ->
          p_h "#include <QtCore/%s>\n" x;
          x
  in
  p_h "\n";
  p_h "class %s: public %s {\n" classname base_classname;
  p_h "  Q_OBJECT\n";
  p_h "  value _camlobjHolder = 0;\n";
  p_h "public:\n";
  (* constructor *)
  p_h "  %s();\n" classname;
  p_h "  void storeCAMLobj(value x) {\n";
  p_h "    if (_camlobjHolder != 0) {\n";
  p_h "       //maybe unregister global root?\n";
  p_h "    }\n";
  p_h "    _camlobjHolder = x;\n";
  p_h "    register_global_root(&_camlobjHolder);\n";
  p_h "  }\n";
  p_c "%s::%s() : _camlobjHolder(0) {\n" classname classname;
  p_c "}\n";

  (* methods *)
  let do_meth ~classname (name,args,res,modif) =
    let (_:Parser.cpptype list) = args in
    let (_:Parser.cpptype) = res in
    let args = if args = [Parser.void_type] then [] else args in
    p_h "  Q_INVOKABLE %s %s(%s)%s;\n" (Parser.string_of_type res) name
      (List.map args ~f:Parser.string_of_type |> String.concat ~sep:",")
      (if List.mem modif `Const then " const" else "");
    (* now source *)
    let locals_count = 1 + (* for _ans *)
      1 + (* camlobj *)
      List.fold_left ~init:0 (res::args)
      ~f:(fun acc x -> max acc TypAst.(x |> of_verbose_typ_exn |> aux_variables_count))
    in
    let argnames_cpp = List.init ~f:(fun n -> sprintf "x%d" n) (List.length args) in
    p_c "//%s: %s\n" name
      (List.map (args@[res]) ~f:(fun x -> x |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
          |> String.concat ~sep:"->"
      );
    p_c "%s %s::%s(%s) %s{\n" (Parser.string_of_type res) classname name
      (let types = List.map args ~f:Parser.string_of_type in
       List.map2_exn ~f:(sprintf "%s %s") types argnames_cpp |> String.concat ~sep:",")
      (if List.mem modif `Const then "const " else "");
    p_c "  CAMLparam0();\n";
    let locals = List.init ~f:(fun n -> sprintf "_x%d" n) (locals_count-1) in
    Qml.print_local_declarations b_c (["_ans"; "_meth"] @ locals);

    (* locals for callback *)
    let make_cb_var = sprintf "_cca%d" in (* generate name *)
    let cb_locals = List.mapi args ~f:(fun i _ -> make_cb_var i) in
    p_c "  CAMLlocalN(_args,%d);\n" (List.length args + 1 (* beacuse of _camlobj *));
    print_local_declarations b_c cb_locals;

    p_c "  qDebug() << \"Calling %s::%s\";\n" classname name;
    p_c "  value _camlobj = this->_camlobjHolder;\n";
    p_c "  Q_ASSERT(Is_block(_camlobj));\n";
    p_c "  Q_ASSERT(Tag_val(_camlobj) == Object_tag);\n";
    p_c "  _meth = caml_get_public_method(_camlobj, caml_hash_variant(\"%s\"));\n" name;
    let (get_var, release_var) = Qml.get_vars_queue locals in (* tail because we need not _ans *)

    let call_closure_str = match List.length args with
      | 0 ->  sprintf "caml_callback2(_meth, _camlobj, Val_unit)"
      | n -> begin
        (* Generating arguments for calling *)
        p_c "  _args[0] = _camlobj;\n";
        List.iter2i args argnames_cpp ~f:(fun i arg cppvar ->
          let ocamlvar = make_cb_var i in
          ocaml_value_of_cpp b_c (get_var,release_var) ~tab:1 ~ocamlvar ~cppvar arg;
          p_c "  _args[%d] = %s;\n" (i+1) ocamlvar
        );
        (*p_c "  value args[%d] = { _camlobj,%s };\n"
          (1+List.length args) (String.concat ~sep:"," cb_locals);*)
        sprintf "caml_callbackN(_meth, %d, _args)" (n+1)
      end
    in
    if res = Parser.void_type then begin
      p_c "  %s;\n" call_closure_str;
      p_c "  CAMLreturn0;\n"
    end else begin
      p_c "  _ans = %s;\n" call_closure_str;
      let cpp_ans_var = "cppans" in
      let new_cpp_var = Qml.getter_of_cppvars "xx" in
      p_c "  %s %s;\n" (Parser.string_of_type res) cpp_ans_var;
      Qml.cpp_value_of_ocaml b_c (get_var,release_var, new_cpp_var) cpp_ans_var "_ans" res;
      p_c "  CAMLreturnT(%s,%s);\n" (string_of_type res) cpp_ans_var;
    end;
    p_c "}\n"
  in
  (* buffers for generating OCaml code *)
  let top_externals_buf = B.create 100 in
  let clas_def_buf = B.create 100 in
  let external_buf = B.create 100 in
  (* Allow to create C++ class from OCaml *)
  bprintf clas_def_buf "class virtual base_%s cppobj = object(self)\n" classname;
  bprintf clas_def_buf "  initializer store cppobj self\n";
  bprintf clas_def_buf "  method handler = cppobj\n";

  (* function to generate properties *)
  let do_prop hbuf cbuf cls_body_buf {name;getter;notifier;typ;setter} =
    let p_h fmt = bprintf hbuf fmt in
    let p_ml fmt = bprintf cls_body_buf fmt in
    let p_ext fmt = bprintf top_externals_buf fmt in
    p_h "public:\n";
    let setter_string = match setter with Some x -> " WRITE "^x | None -> " " in
    p_h "  Q_PROPERTY(%s %s%s READ %s NOTIFY %s)\n"
      (string_of_type typ) name setter_string getter notifier;
    do_meth ~classname (getter,[void_type],typ,[]);
    let () =
      match setter with
        | Some setter ->
            do_meth ~classname (setter,[typ],bool_type,[]);
        | None -> ()
    in
    p_h "signals:\n";
    p_h "  void %s(%s);\n" notifier (string_of_type typ);
    p_h "public:\n";
    p_h "  void emit_%s(%s arg1) {\n" notifier (Parser.string_of_type typ);
    p_h "    emit %s(arg1);\n" notifier;
    p_h "  }\n\n";
    let stub_name = gen_cppmeth_wrapper ~classname cbuf (notifier,[typ],void_type,[]) in
    p_ext "external stub_%s: cppobj -> %s -> unit = \"%s\"\n" notifier
      (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
      stub_name;

    p_ml "  method emit_%s = stub_%s self#handler\n" notifier notifier;
    p_ml "  method virtual %s: unit -> %s\n" getter
      (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
  in

  let do_meth_caml (name,args,res,_) =
    let caml_types = List.map args ~f:(fun x -> x |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type) in
    bprintf clas_def_buf "  method virtual %s: %s %s\n" name
      (if args=[] then "" else (String.concat ~sep:"->" caml_types)^"->")
      (res |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
  in

  List.iter members ~f:(fun mem ->
    do_meth ~classname mem;
    do_meth_caml mem;
  );
  List.iter props ~f:(do_prop b_h b_c clas_def_buf);

  (* Now we will add some methods for specific basename *)
  let () =
    if base_classname = "QAbstractItemModel" then begin
      let model_members = qabstractItemView_members in
      List.iter model_members ~f:(do_meth ~classname);
      p_h "private:\n";
      p_h "  QHash<int, QByteArray> _roles;\n";
      p_h "public:\n";
      p_h "  QModelIndex makeIndex(int row,int column) {\n";
      p_h "    if (row==-1 || column==-1)\n";
      p_h "      return QModelIndex();\n";
      p_h "    else\n";
      p_h "      return createIndex(row,column,(void*)NULL);\n";
      p_h "  }\n";

      p_h "Q_INVOKABLE QList<QString> roles() {\n";
      p_h "  QList<QString> ans;\n";
      p_h "  foreach(QByteArray b, _roles.values() )\n";
      p_h "      ans << QString(b);\n";
      p_h "  return ans;\n";
      p_h "}\n";
      p_h "void addRole(int r, QByteArray name) { _roles.insert(r,name); }\n";
      p_h "virtual QHash<int, QByteArray> roleNames() const { return _roles; }\n";
      (* signal to report changing of data *)
      p_h "void emit_dataChanged(int a, int b, int c, int d) {\n";
      p_h "  const QModelIndex topLeft     = createIndex(a,b);\n";
      p_h "  const QModelIndex bottomRight = createIndex(c,d);\n";
      p_h "  emit dataChanged(topLeft, bottomRight);\n";
      p_h "}\n";

      (* next methods declared in C++ and are not overridable in OCaml *)
      let cpp_wrap_stubs =
        [ (("dataChanged",[Parser.qmodelindex_type;Parser.qmodelindex_type],Parser.void_type,[]),
           "stub_report_dataChanged", "report_dataChanged")
        ; (("beginInsertRows",[qmodelindex_type;int_type;int_type],void_type,[]),
           "stub_beginInsertRows", "beginInsertRows")
        ; (("endInsertRows",[void_type],void_type,[]),
           "stub_endInsertRows", "endInsertRows")
        ; (("beginRemoveRows",[qmodelindex_type;int_type;int_type],void_type,[]),
           "stub_beginRemoveRows", "beginRemoveRows")
        ; (("endRemoveRows",[void_type],void_type,[]),
           "stub_endRemoveRows", "endRemoveRows")
        ]
      in
      List.iter cpp_wrap_stubs ~f:(fun ((_,args,res,_) as desc,stub_name,methname) ->
        let cpp_stub_name = gen_cppmeth_wrapper ~classname b_c desc in
        bprintf top_externals_buf
          "external %s: cppobj -> %s =\n  \"%s\"\n" stub_name
          (List.map (args@[res]) ~f:(fun x -> x |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
           |> String.concat ~sep:"->")
          cpp_stub_name;
        bprintf clas_def_buf " method %s = %s cppobj\n" methname stub_name
      );

      List.iter qabstractItemView_members ~f:(do_meth_caml);
      (* stub for adding new roles *)
      let add_role_stub_name =
        gen_cppmeth_wrapper ~classname b_c
          ("addRole", [int_type; bytearray_type |> unreference], void_type, []) in
      bprintf external_buf
        "external add_role: 'a -> int -> string -> unit = \"%s\"\n" add_role_stub_name
    end
  in

  (* Also we need to have a stubs to create C++ class *)
  p_c "extern \"C\" value caml_create_%s(value _dummyUnitVal) {\n" classname;
  p_c "  CAMLparam1(_dummyUnitVal);\n";
  p_c "  CAMLlocal1(_ans);\n";
  p_c "  _ans = caml_alloc_small(1, Abstract_tag);\n";
  p_c "  (*((%s **) &Field(_ans, 0))) = new %s();\n" classname classname;
  p_c "  CAMLreturn(_ans);\n";
  p_c "}\n";

  p_c "extern \"C\" value caml_store_value_in_%s(value _cppobj,value _camlobj) {\n" classname;
  p_c "  CAMLparam2(_cppobj,_camlobj);\n";
  p_c "  %s *o = (%s*) (Field(_cppobj,0));\n" classname classname;
  p_c "  o->storeCAMLobj(_camlobj); // register global root in member function\n";
  p_c "  //caml_register_global_root(&(o->_camlobjHolder));\n";
  p_c "  CAMLreturn(Val_unit);\n";
  p_c "}\n";

  p_h "};\n";
  p_h "#endif // %s_H\n" classname;

  bprintf clas_def_buf "end\n";
  bprintf external_buf "external create_%s: unit -> 'a = \"caml_create_%s\"\n" classname classname ;
  bprintf top_externals_buf "external store: cppobj -> < .. > -> unit = \"%s\"\n"
     (sprintf "caml_store_value_in_%s" classname);

  p_ml "%s\n" (B.contents top_externals_buf);
  p_ml "%s\n" (B.contents clas_def_buf);
  p_ml "%s\n" (B.contents external_buf);

  with_file (directory ^/ classname ^ "_c.cpp") (fun file -> B.output_buffer file b_c);
  with_file (directory ^/ classname ^ "_c.h")   (fun file -> B.output_buffer file b_h);
  with_file (directory ^/ classname ^ ".ml")  (fun file -> B.output_buffer file b_ml)
