open Core_kernel
open Core_kernel.Std
open Parser
open Qml

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
let gen_cppmeth_wrapper ~classname ?(config=[]) (cbuf: Bigbuffer.t) meth =
  let p_c fmt = Bigbuffer.Printf.bprintf cbuf fmt in
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
  if List.mem config `PrintMethCalls then
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

let wrap ~classname  ~config do_meth do_meth_caml b_h b_c external_buf top_externals_buf clas_def_buf =
  let open Bigbuffer.Printf in
  let p_h   fmt = bprintf b_h fmt in
  (*let p_c   fmt = bprintf b_c fmt in*)

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
        let cpp_stub_name = gen_cppmeth_wrapper ~config ~classname b_c desc in
        Bigbuffer.Printf.bprintf top_externals_buf
          "external %s: cppobj -> %s =\n  \"%s\"\n" stub_name
          (List.map (args@[res]) ~f:(fun x -> x |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
           |> String.concat ~sep:"->")
          cpp_stub_name;
        bprintf clas_def_buf " method %s = %s cppobj\n" methname stub_name
      );

      List.iter qabstractItemView_members ~f:(do_meth_caml);
      (* stub for adding new roles *)
      let add_role_stub_name =
        gen_cppmeth_wrapper ~classname b_c ~config
          ("addRole", [int_type; bytearray_type |> unreference], void_type, []) in
      bprintf external_buf
        "external add_role: 'a -> int -> string -> unit = \"%s\"\n" add_role_stub_name
