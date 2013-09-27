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


let generate ?(directory=".") ?(config=[]) {classname; basename; members; slots; props; signals} =
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
  (*p_ml "\nopen QmlContext\nopen QtGraphics\n\n";*)

  p_h "#ifndef %s_c_H\n" classname;
  p_h "#define %s_c_H\n" classname;
  p_c "#include \"%s_c.h\"\n\n" classname;

  p_h "#include \"kamlo.h\"\n";
  p_h "#include <QtCore/QDebug>\n";
  p_h "#include <QtCore/QObject>\n";
  p_h "#include <QtWidgets/QGraphicsSceneMouseEvent>\n";
  p_h "#include <QtGui/QKeyEvent>\n";

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
  p_h "  value _camlobjHolder;\n";
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
    if List.mem config `PrintMethCalls then
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

  let p_ext fmt = bprintf top_externals_buf fmt in
  let declare_cpp_signal hbuf ~name ~args =
    let p_h fmt = bprintf hbuf fmt in
    let types = List.map ~f:string_of_type args in
    let s1 = List.mapi types ~f:(fun i t -> sprintf "%s arg%d" t i) |> String.concat ~sep:"," in
    let s2 = List.mapi types ~f:(fun i _ -> sprintf "arg%d" i) |> String.concat ~sep:"," in
    p_h "signals:\n";
    p_h "  void %s(%s);\n" name (String.concat ~sep:"," types);
    p_h "public:\n";
    p_h "  void emit_%s(%s) {\n" name s1;
    p_h "    emit %s(%s);\n" name s2;
    p_h "  }\n\n"
  in
  (* function to generate properties *)
  let do_prop hbuf cbuf cls_body_buf {name;getter;notifier;typ;setter} =
    let p_h fmt = bprintf hbuf fmt in
    let p_ml fmt = bprintf cls_body_buf fmt in
    p_h "public:\n";
    let setter_string = match setter with Some x -> " WRITE "^x | None -> " " in
    let prop_descr_str = sprintf "Q_PROPERTY(%s %s%s READ %s NOTIFY %s)"
      (string_of_type typ) name setter_string getter notifier in
    p_h "  %s\n" prop_descr_str;
    do_meth ~classname (getter,[void_type],typ,[]);
    declare_cpp_signal hbuf ~name:notifier ~args:[typ];
    let stub_name = WrapAbstractItemModel.gen_cppmeth_wrapper
      ~config ~classname cbuf (notifier,[typ],void_type,[]) in
    p_ext "external stub_%s: cppobj -> %s -> unit = \"%s\"\n" notifier
      (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type)
      stub_name;

    bprintf clas_def_buf "  (* %s *)\n" prop_descr_str;
    let () =
      match setter with
        | Some setter ->
            do_meth ~classname (setter,[typ],bool_type,[]);
            bprintf clas_def_buf "  method virtual %s: %s -> unit\n" setter
              (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type);
        | None -> ()
    in

    p_ml "  method emit_%s = stub_%s self#handler\n" notifier notifier;
    p_ml "  method virtual %s: unit -> %s\n" getter
      (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type);
    let () = match setter with
      | Some setter ->
          let (tt,convt) =
            if typ = Parser.int_type then "int","of_int"
            else if typ = Parser.qpoint_type then "QPoint","of_qpoint"
            else if typ = Parser.string_type then "string","of_string"
            else failwith "only QVariant-compatible types can be in properties"
          in
          p_ml "  method prop_%s = object\n" name;
          p_ml "    inherit [%s] qvariant_prop \"%s\" as base\n" tt name;
          p_ml "    method get = self#%s ()\n" getter;
          p_ml "    method set x = self#%s x\n" setter;
          p_ml "    method wrap_in_qvariant x = QVariant.%s x\n" convt;
          p_ml "  end\n";
      | None  ->
          p_ml "  (* don't decare properties without setter *)\n"
    in
    ()
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
  List.iter slots ~f:(fun ((name,args,_,_) as mem) ->
    p_h "public slots:\n";
    do_meth ~classname mem;
    let f x = x |> TypAst.of_verbose_typ_exn |> TypAst.to_cpp_type in
    let slot_string = sprintf "1%s(%s)" name (String.concat ~sep:","(List.map ~f args)) in
    bprintf clas_def_buf
      "  method slot_%s : Slot.t = Slot.create (Obj.magic self#handler) \"%s\"\n" name slot_string;
    do_meth_caml mem;
  );
  List.iter props ~f:(do_prop b_h b_c clas_def_buf);

  List.iter signals ~f:(fun (name,args) ->
    declare_cpp_signal b_h ~name ~args;
    let stub_name = WrapAbstractItemModel.gen_cppmeth_wrapper
      ~config ~classname b_c ("emit_"^name,args,void_type,[]) in
    p_ext "external stub_%s: cppobj %s -> unit = \"%s\"\n" name
      (List.map ~f:(fun typ -> sprintf "-> %s "
        (typ |> TypAst.of_verbose_typ_exn |> TypAst.to_ocaml_type) ) args |> String.concat ~sep:" ")
      stub_name;
    bprintf clas_def_buf "  method emit_%s = stub_%s self#handler\n" name name
  );

  (* Now we will add some methods for specific basename *)
  let () =
    if base_classname = "QAbstractItemModel" then
      WrapAbstractItemModel.wrap ~classname ~config do_meth do_meth_caml
        b_h b_c external_buf top_externals_buf clas_def_buf
    else ()
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
