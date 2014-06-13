open Printf

module List = struct
  include ListLabels
  let rec list_last = function
    | [] -> failwith "Bad argument of list_last"
    | [last] -> ([],last)
    | x::xs -> list_last xs |> (fun (xs,last) -> (x::xs,last))

  let to_string ~f xs = String.concat "," (List.mapi f xs)
  let init f n =
    let rec helper acc = function
      | x when x<0 -> acc
      | x -> helper (f x :: acc) (x-1)
    in
    helper []  (n-1)
  let find ~f xs =
    try Some(find ~f xs) with Not_found -> None

end

module Time = struct
  let now () = Unix.(localtime @@ time() )
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  let str_of_month n =
    if n>=0 && n<=12 then months.(n)
    else failwith "Wrong argument of str_of_month"
  let to_string {Unix.tm_sec; Unix.tm_mon; Unix.tm_min; Unix.tm_hour; Unix.tm_mday; Unix.tm_year } =
    sprintf "%02d %s, %d %02d:%02d:%02d"
            tm_mday (str_of_month tm_mon) (1900+tm_year) tm_hour tm_min tm_sec
end


let printfn fmt = kprintf (printf "%s\n") fmt
let fprintfn ch fmt = ksprintf (fprintf ch "%s\n") fmt

let print_time ch =
  fprintfn ch "/*";
  fprintfn ch " * Generated at %s" Time.(now () |> to_string);
  fprintfn ch " */"

module Ref = struct
  let append ~set x = set := x :: !set
  let replace x f = (x := f !x)
end

module FilesKey = struct
  type ext = CSRC | CHDR
  type t = string * ext
  let cmp_string : string -> string -> int = compare
  let compare a b = match a,b with
    | (_,CSRC),(_,CHDR) -> -1
    | (_,CHDR),(_,CSRC) ->  1
    | (x,_),(y,_) -> cmp_string x y
end
module FilesMap = Map.Make(FilesKey)

let files = ref FilesMap.empty

let open_files ?(destdir=".") ?(ext="cpp") ~options ~classname =
  (*print_endline "Opening files....";*)
  let src = open_out (sprintf "%s/%s_c.%s" destdir classname ext) in
  let hdr = open_out (sprintf "%s/%s.h" destdir classname) in
  print_time hdr;
  let println fmt = fprintfn hdr fmt in
  fprintfn hdr "#ifndef %s_H" (String.uppercase classname);
  fprintfn hdr "#define %s_H" (String.uppercase classname);
  fprintfn hdr "";
  fprintfn hdr "#include <QtCore/QDebug>";
  fprintfn hdr "#include <QtCore/QObject>";
  fprintfn hdr "#include <QtCore/QAbstractItemModel>";
  println "";
  println "#ifdef __cplusplus";
  println "extern \"C\" {";
  println "#endif";
  fprintfn hdr "#include <caml/alloc.h>";
  fprintfn hdr "#include <caml/mlvalues.h>";
  fprintfn hdr "#include <caml/callback.h>";
  fprintfn hdr "#include <caml/memory.h>";
  fprintfn hdr "#include <caml/threads.h>";
  println "#ifdef __cplusplus";
  println "}";
  println "#endif";
  println "";
  fprintfn hdr "class %s : public %s {" classname
           (if List.mem `ItemModel ~set:options then "QAbstractItemModel" else "QObject");
  fprintfn hdr "  Q_OBJECT";
  fprintfn hdr "  value _camlobjHolder; // store reference to OCaml value there";
  fprintfn hdr "public:";
  fprintfn hdr "  %s() : _camlobjHolder(0) { };" classname;
  fprintfn hdr "  void storeCAMLobj(value x) {";
  fprintfn hdr "    if (_camlobjHolder != 0) {";
  fprintfn hdr "       //maybe unregister global root?";
  fprintfn hdr "    }";
  fprintfn hdr "    _camlobjHolder = x;";
  fprintfn hdr "    register_global_root(&_camlobjHolder);";
  fprintfn hdr "  }\n";
  let () =
    if List.mem `ItemModel ~set:options then (
      println "private:";
      println "  QHash<int, QByteArray> _roles;";
      println "public:";
      println "  QModelIndex makeIndex(int row,int column) const {";
      println "    if (row==-1 || column==-1)";
      println "      return QModelIndex();";
      println "    else";
      println "      return createIndex(row,column,(void*)NULL);";
      println "  }";
      println "  QList<QString> roles() {";
      println "    QList<QString> ans;";
      println "    foreach(QByteArray b, _roles.values() )";
      println "      ans << QString(b);";
      println "    return ans;";
      println "  }";
      println "  void addRole(int r, QByteArray name) { _roles.insert(r,name); }";
      println "  virtual QHash<int, QByteArray> roleNames() const { return _roles; }";
      println "  void emit_dataChanged(int a, int b, int c, int d) {";
      println "    const QModelIndex topLeft     = createIndex(a,b);";
      println "    const QModelIndex bottomRight = createIndex(c,d);";
      println "    emit dataChanged(topLeft, bottomRight);";
      println "  }";
      println "";

    )
  in

  files := FilesMap.(add (classname, FilesKey.CHDR) hdr !files);
  print_time src;
  fprintfn src "#include \"%s.h\"" classname;
  fprintfn src "";
  files := FilesMap.(add (classname, FilesKey.CSRC) src !files);
  ()

let enter_blocking_section ch =
  fprintfn ch "  caml_release_runtime_system();";
  ()

let leave_blocking_section ch =
  fprintfn ch "  caml_acquire_runtime_system();";
  ()

let close_files () =
  (*print_endline "Closing files";*)
  let f (classname,ext) hndl =
    let println fmt = fprintfn hndl fmt in
    match ext with
    | FilesKey.CHDR ->
       println "};";
       println "#endif /* %s_H */\n" (String.uppercase classname);
       println "extern \"C\" value caml_create_%s(value _dummyUnitVal);" classname;
       println "extern \"C\" value caml_store_value_in_%s(value _cppobj,value _camlobj);" classname;
       close_out hndl
    | FilesKey.CSRC ->
       (* we need to generate stubs for creating C++ object there *)
       println "extern \"C\" value caml_create_%s(value _dummyUnitVal) {" classname;
       println "  CAMLparam1(_dummyUnitVal);";
       println "  CAMLlocal1(_ans);";
       enter_blocking_section hndl;
       println "  _ans = caml_alloc_small(1, Abstract_tag);";
       println "  (*((%s **) &Field(_ans, 0))) = new %s();" classname classname;
       leave_blocking_section hndl;
       println "  CAMLreturn(_ans);";
       println "}\n";

       println "extern \"C\" value caml_store_value_in_%s(value _cppobj,value _camlobj) {" classname;
       println "  CAMLparam2(_cppobj,_camlobj);";
       enter_blocking_section hndl;
       println "  %s *o = (%s*) (Field(_cppobj,0));" classname classname;
       println "  o->storeCAMLobj(_camlobj);";
       leave_blocking_section hndl;
       println "  CAMLreturn(Val_unit);";
       println "}";
       close_out hndl
  in
  FilesMap.iter f !files;
  files := FilesMap.empty

module Names = struct
  let signal_of_prop s = s^"Changed"
  let getter_of_prop s = "get"^s
  let setter_of_prop s = "set"^s
end

exception VaribleStackEmpty

let get_vars_queue xs =
  let stack = ref xs in
  let get_var () = match !stack with
    | x::xs -> stack:= xs; x
    | [] -> raise VaribleStackEmpty
  in
  let release_var name = Ref.replace stack (fun xs -> name::xs) in
  (get_var,release_var)

let getter_of_cppvars  prefix =
  let last_index = ref 0 in
  let f () =
    incr last_index;
    sprintf "%s%d" prefix !last_index
    in
    f

(* TODO: add addtitional info about methods *)
(* We need this arginfo because void foo(QString) is not the same as void foo(const QString&) *)
type arg_info = { ai_ref: bool; ai_const: bool }
(* properties can have only simple types (except unit) *)
type prop_typ = [ `bytearray | `bool | `int | `string | `list of prop_typ ]
type meth_typ_item =
  [ `unit | `bool | `int  | `string | `list of meth_typ_item
  | `modelindex | `variant | `bytearray
  ]
type meth_typ = (meth_typ_item*arg_info) list
type meth_info = { mi_virt: bool; mi_const: bool }

let wrap_typ_simple x = (x,{ ai_ref=false; ai_const=false })
let unref   (x,{ai_ref;ai_const}) = (x, {ai_ref=false;ai_const})
let unconst (x,{ai_ref;ai_const}) = (x, {ai_ref;ai_const=false})


(* how many additional variables needed to convert C++ value to OCaml one *)
let aux_variables_count (y: meth_typ_item) =
  let rec helper = function
    | `variant
    | `bytearray
    | `bool | `int | `unit | `string | `modelindex -> 0
    | `list x -> helper x + 2
  in
  helper y

(* how many additional variables needed to convert OCaml value to C++ one *)
let aux_variables_count_to_cpp (y: meth_typ_item) =
  let rec helper = function
    | `variant
    | `bytearray
    | `bool | `int | `unit | `string | `modelindex -> 0
    | `list x -> helper x + 2
  in
  helper y

let rec ocaml_ast_of_typ x :  Longident.t =
  let open Longident in
  match x with
  | `cppobj    -> Lident "cppobj"
  | `variant   -> Ldot (Lident "QVariant",   "t")
  | `modelindex-> Ldot (Lident "QModelIndex","t")
  | `bool      -> Lident "bool"
  | `unit      -> Lident "unit"
  | `bytearray
  | `string    -> Lident "string"
  | `int       -> Lident "int"
  | `list x    -> Lapply (Lident "list", ocaml_ast_of_typ x)

let rec cpptyp_of_proptyp: prop_typ*arg_info -> string = fun (typ,ai) ->
  sprintf "%s%s%s" (if ai.ai_const then "const " else "")
          (match typ with
           | `bool -> "bool"
           | `int  -> "int"
           | `bytearray -> "QByteArray"
           | `string    -> "QString"
           | `list x -> sprintf "QList<%s>" (cpptyp_of_proptyp @@ wrap_typ_simple x))
          (if ai.ai_ref then "&" else "")

let cpptyp_of_typ: _*_ -> string = fun (x,ai) ->
  let rec helper (x,ai) =
    match x with
    | `bytearray
    | `bool  | `int  | `string as x -> cpptyp_of_proptyp (x,ai)
    | `unit    -> "void"
    | `variant -> "QVariant"
    | `modelindex -> sprintf "%sQModelIndex%s" (if ai.ai_const then "const " else "")
                             (if ai.ai_ref then "&" else "")
    | `list x -> sprintf "%sQList<%s>%s" (if ai.ai_const then "const " else "")
                         (helper (x,{ai_ref=false;ai_const=false}))
                         (if ai.ai_ref then "&" else "")
  in
  let (_: meth_typ_item*_ ->_) = helper in
  match x with
  | `cppobj  -> failwith "Bug. cppobj can't appear in C++"
  | `bool    -> helper (`bool,ai)
  | `bytearray-> helper (`bytearray,ai)
  | `int     -> helper (`int, ai)
  | `unit    -> helper (`unit, ai)
  | `variant -> helper (`variant, ai)
  | `list a  -> helper (`list a,ai)
  | `string  -> helper (`string,ai)
  | `modelindex -> helper (`modelindex,ai)

let print_declarations ?(mode=`Local) ch xs =
  let m = match mode with `Local -> "CAMLlocal" | `Param -> "CAMLparam" in
  let rec helper = function
  | a::b::c::d::e::xs ->
     fprintf ch "  %s5(%s);\n" m (String.concat "," [a;b;c;d;e]);
     helper xs
  | [] -> ()
  | xs ->
     let n = List.length xs in
     assert (n<5);
     fprintf ch "  %s%d(%s);\n" m n (String.concat "," xs)
  in
  helper xs

let print_local_declarations ch xs = print_declarations ~mode:`Local ch xs
let print_param_declarations ch xs = print_declarations ~mode:`Param ch xs

let cpp_value_of_ocaml ?(options=[]) ~cppvar ~ocamlvar
                       ch (get_var,release_var,new_cpp_var) typ =
  let rec helper ~tab dest var typ =
    let prefix = String.make (2*tab) ' ' in
    let println fmt = fprintf ch "%s" prefix; fprintfn ch fmt in
    match typ with
    | `unit      -> ()
    | `int       -> println "%s = Int_val(%s);" dest ocamlvar
    | `string    -> println "%s = QString(String_val(%s));" dest ocamlvar
    | `bytearray -> println "%s = QByteArray(String_val(%s));" dest ocamlvar
    | `bool      -> println "%s = Bool_val(%s);" dest ocamlvar
    | `modelindex ->
       begin
         match List.find options ~f:(function `ItemModel _ -> true) with
         | Some (`ItemModel obj) ->
            let call =
              match obj with
              | Some o -> sprintf "%s->makeIndex" o
              | None  -> "createIndex"
            in
            println "%s = %s(Int_val(Field(%s,0)), Int_val(Field(%s,1)) );"
                    dest call ocamlvar ocamlvar
         | None -> failwith "QModelIndex is not available without QAbstractItemModel base"
       end
    | `variant ->
       println "if (Is_block(%s)) {" var;
       println "  if (caml_hash_variant(\"string\") == Field(%s,0))" var;
       println "    %s = QVariant::fromValue(QString(String_val(Field(%s,1))));" dest var;
       println "  else if(caml_hash_variant(\"int\") == Field(%s,0))" var;
       println "    %s = QVariant::fromValue(Int_val(Field(%s,1)));" dest var;
       println "  else if(caml_hash_variant(\"qobject\") == Field(%s,0))" var;
       println "    %s = QVariant::fromValue((QObject*) (Field(Field(%s,1),0)));" dest var;
       println "  else Q_ASSERT_X(false,\"%s\",\"%s\");"
                 "While converting OCaml value to QVariant"
                 "Unknown variant tag";
       println "} else { // empty QVariant";
       println "    %s = QVariant();" dest;
       println "}"
    | `list t->
       let cpp_typ_str =
         let u : [`cppobj|meth_typ_item] * _ =
           ((wrap_typ_simple typ) :> ([`cppobj|meth_typ_item]*arg_info)) in
         cpptyp_of_typ u
       in
       let cpp_argtyp_str = cpptyp_of_typ @@
                              ((wrap_typ_simple t) :> ([`cppobj|meth_typ_item]*arg_info) )
       in
       println "// generating %s" cpp_typ_str;
       let temp_var = get_var () in
       let head_var = get_var () in
       let temp_cpp_var = new_cpp_var () in
       println "%s = %s;\n" temp_var var;
       println "while (%s != Val_emptylist) {\n" temp_var;
       println "  %s = Field(%s,0); /* head */"  head_var temp_var;
       println "  %s %s;" cpp_argtyp_str temp_cpp_var;
       helper  ~tab:(tab+1) temp_cpp_var head_var t;
       println "  %s << %s;\n" dest temp_cpp_var;
       println "  %s = Field(%s,1);" temp_var temp_var;
       println "}";
       release_var head_var;
       release_var temp_var
  in
  helper ~tab:1 cppvar ocamlvar typ

let ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar typ =
  let rec helper ~tab ~var ~dest typ =
    let println fmt = fprintf ch "%s" (String.make (2*tab) ' '); fprintfn ch fmt in
    match typ with
    | `unit       -> failwith "Can't generate OCaml value from C++ void a.k.a. unit"
    | `int        -> println "%s = Val_int(%s);" dest var
    | `bool       -> println "%s = Val_bool(%s);" dest var
    | `string     -> println "%s = caml_copy_string(%s.toLocal8Bit().data());" dest var
    | `bytearray  -> println "%s = caml_copy_string(%s.toLocal8Bit().data());" dest var
    | `modelindex ->
       println "%s = caml_alloc(2,0);" dest;
       println "Store_field(%s,0,Val_int(%s.row()));" dest var;
       println "Store_field(%s,1,Val_int(%s.column()));" dest var
    | `variant ->
       println "if (!%s.isValid())\n" var;
       println "  %s=hash_variant(\"empty\");" var;
       println "else if(%s.type() == QMetaType::QString) {" var;
       println "  %s = caml_alloc(2,0);" dest;
       println "  Store_field(%s,0,%s);" dest "hash_variant(\"string\")";
       println "  Store_field(%s,1,%s);" dest
                 (sprintf "caml_copy_string(%s.value<QString>().toLocal8Bit().data()" var);
       println "else if (%s.type() == QMetaType::User) {" var;
       println "else {";
       println "  Q_ASSERT_X(false,\"qVariant_of_cpp\",\"not all cases are supported\");";
       println "}"

    | `list t ->
       let cons_helper = get_var () in
       let cons_arg_var = get_var () in
       println "%s = Val_emptylist;\n" dest;
       println "if ((%s).length() != 0) {" var;
       println "  auto it = (%s).end() - 1;" var;
       println "  for (;;) {";
       println "    %s = caml_alloc(2,0);" cons_helper;
       helper ~tab:(tab+1) ~var:"(*it)" ~dest:cons_arg_var t;
       println "    Store_field(%s, 0, %s);" cons_helper cons_arg_var;
       println "    Store_field(%s, 1, %s);" cons_helper dest;
       println "    %s = %s;" dest cons_helper;
       println "    if ((%s).begin() == it) break;" var;
       println "    it--;";
       println "  }";
       println "}";
       release_var cons_arg_var;
       release_var cons_helper;
       println "";
    ()
  in
  helper ~tab:1 ~var:cppvar ~dest:ocamlvar typ

(* stub implementation to call it from OCaml *)
let gen_stub_cpp ?(options=[]) ~classname ~stubname ~methname ch
                 (types: (meth_typ_item * arg_info) list)=
  let println fmt = fprintfn ch fmt in
  let (args,res) = List.list_last types in
  let res = unref res in
  println "// stub: %s name(%s)" (cpptyp_of_typ res) (List.to_string ~f:(fun _ -> cpptyp_of_typ) args);
  let argnames = List.mapi ~f:(fun i _ -> sprintf "_x%d" i) args in
  let cppvars  = List.mapi ~f:(fun i _ -> sprintf "z%d" i) args in
  println "extern \"C\" value %s(%s) {"
          stubname
          (match args with
           (*| [(`unit,_)] -> "value _cppobj"*)
           | _ -> List.map ~f:(sprintf "value %s") ("_cppobj"::argnames) |> String.concat ",");
  print_param_declarations ch ("_cppobj"::argnames);
  let aux_count =
    List.fold_left ~f:(fun acc x -> max (aux_variables_count_to_cpp @@ fst x) acc) ~init:0 args
  in
  let aux_count = max aux_count (aux_variables_count @@ fst res) in
  println "  // aux vars count = %d" aux_count;
  let local_names = List.init (sprintf "_x%d") aux_count in
  print_local_declarations ch local_names;

  enter_blocking_section ch;
  println "  %s *o = (%s*) (Field(_cppobj,0));" classname classname;

  let get_var,release_var = get_vars_queue local_names in
  let cpp_var_counter = ref 0 in
  let new_cpp_var () = incr cpp_var_counter; sprintf "zz%d" !cpp_var_counter in

  let options = if List.mem `ItemModel ~set:options then [`ItemModel (Some "o")] else [] in
  let f = fun i arg ->
    let cppvar = sprintf "z%d" i in
    let ocamlvar = sprintf "_x%d" i in
    println "  %s %s;" (cpptyp_of_typ arg) cppvar;
    cpp_value_of_ocaml ch ~options ~cppvar ~ocamlvar (get_var,release_var,new_cpp_var) (fst arg)
  in
  List.iteri ~f args;
  let () = match res with
    | (`unit,_) ->
       println "  o->%s(%s);" methname (String.concat "," cppvars);
       leave_blocking_section ch;
       println "  CAMLreturn(Val_unit);";
    | (t, ai)   ->
       let cppvar = "res" in
       println "  %s %s = o->%s(%s);" (cpptyp_of_typ res) cppvar methname (String.concat "," cppvars);
       let ocamlvar = "_ans" in
       fprintf ch "  ";
       ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar (fst res);
       leave_blocking_section ch;
       println "  CAMLreturn(%s);" ocamlvar
  in
  println "}";
  ()

(* method implementation from class header. Used for invacation OCaml from C++ *)
let gen_meth_cpp ~minfo ?(options=[]) ~classname ~methname ch (types: meth_typ) =
  let println fmt = fprintfn ch fmt in
  let print   fmt = fprintf  ch fmt in
  fprintfn ch "// %s::%s: %s" classname methname (List.to_string ~f:(fun _ -> cpptyp_of_typ) types);
  let (args,res) = List.list_last types in
  let res = unconst @@ unref res in
  if fst res=`unit
  then print "void "
  else print "%s " (cpptyp_of_typ res);

  println "%s::%s(%s) %s {" classname methname
          (match args with
           | [(`unit,_)] -> ""
           | _ -> List.to_string ~f:(fun i t -> sprintf "%s x%d" (cpptyp_of_typ t) i) args)
          (if minfo.mi_const then " const" else "");
  println "  CAMLparam0();";
  let locals_count = 2 +
    List.fold_left ~f:(fun acc (x,_) -> max acc (aux_variables_count x)) ~init:0 types
  in
  let locals = List.init (sprintf "_x%d") (locals_count-1) in
  print_local_declarations ch ("_ans" :: "_meth" :: locals);
  (* array for invoking OCaml method *)
  println "  CAMLlocalN(_args,%d);" (List.length args + 1);
  (*println "  // aux vars count = %d" locals_count; *)
  let make_cb_var = sprintf "_cca%d" in (* generate name *)
  let cb_locals = List.mapi ~f:(fun i _ -> make_cb_var i) args in
  print_local_declarations ch cb_locals;
  leave_blocking_section ch;

  println "  value _camlobj = this->_camlobjHolder;";
  println "  Q_ASSERT(Is_block(_camlobj));";
  println "  Q_ASSERT(Tag_val(_camlobj) == Object_tag);";
  println "  _meth = caml_get_public_method(_camlobj, caml_hash_variant(\"%s\"));" methname;

  let get_var,release_var = get_vars_queue locals in
  let call_closure_str = match List.length args with
    | 0
    | 1 when (match args with [`unit,_] -> true | _ -> false) ->
       sprintf "caml_callback2(_meth, _camlobj, Val_unit);"
    | n ->
       println "  _args[0] = _camlobj;";
       let f i (typ,_) =
         let cppvar = sprintf "x%d" i in
         let ocamlvar = make_cb_var i in
         fprintf ch "  ";
         (*fprintfn stdout "call ocaml_value_of_cpp %s" (cpptyp_of_typ arg);*)
         ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar typ;
         println "  _args[%d] = %s;" (i+1) ocamlvar
       in
       List.iteri ~f  args;
       sprintf "caml_callbackN(_meth, %d, _args);" (n+1)
  in
  if fst res = `unit then (
    println "  %s" call_closure_str;
    enter_blocking_section ch;
    println "  CAMLreturn0;"
  )else(
    let options = [`ItemModel (Some "this")] in
    let ocamlvar = "_ans" in
    let cpp_res_typ = cpptyp_of_typ res in
    println "  %s = %s;" ocamlvar call_closure_str;
    enter_blocking_section ch;
    let cppvar = "cppans" in
    println "  %s %s;" cpp_res_typ cppvar;
    let new_cpp_var = getter_of_cppvars "xx" in
    cpp_value_of_ocaml ~options ~cppvar ~ocamlvar ch (get_var,release_var, new_cpp_var) (fst res);
    println "  CAMLreturnT(%s,%s);" cpp_res_typ cppvar;
  );
  println "}";
  ()

let gen_prop ~classname ~propname (typ: prop_typ) =
  (*printf "Generation prop '%s' of class '%s'.\n" propname classname;*)
  let println fmt =
    let hndl = FilesMap.find (classname,FilesKey.CHDR) !files in
    fprintfn hndl fmt
  in
  let sgnl_name = Names.signal_of_prop propname in
  let getter_name = Names.getter_of_prop propname in
  (*let setter_name = Names.setter_of_prop propname in*)
  let cpptyp_name = cpptyp_of_proptyp @@ wrap_typ_simple typ in

  println "public:";
  println "  Q_PROPERTY(%s %s READ %s NOTIFY %s)" cpptyp_name propname getter_name sgnl_name;
  println "  Q_INVOKABLE %s %s();" cpptyp_name getter_name;
  println "signals:";
  println "  void %s(%s %s);" sgnl_name cpptyp_name propname;
  (* C++ part now *)
  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
  (*println "// Q_PROPERTY( %s )" propname;*)
  gen_meth_cpp ~classname ~methname:getter_name hndl ~minfo:{mi_const=false;mi_virt=false}
               (* TODO: maybe we can use cosnt and & for setter argument *)
               [ ((`unit :> meth_typ_item), {ai_ref=false;ai_const=false})
               ; ((typ   :> meth_typ_item), {ai_ref=false;ai_const=false})
               ];
  let stubname: string  = sprintf "caml_%s_%s_cppmeth_wrapper" classname sgnl_name in
  gen_stub_cpp ~classname ~methname:sgnl_name ~stubname
               hndl
               [ ((typ   :> meth_typ_item), {ai_ref=false;ai_const=false})
               ; ((`unit :> meth_typ_item), {ai_ref=false;ai_const=false})
               ];
  ()

let gen_meth ?(minfo={mi_virt=false; mi_const=false}) ?(options=[]) ~classname ~methname typ =
  (*printf "Generation meth '%s' of class '%s'.\n" methname classname;*)
  let typ' = typ |> List.map ~f:(function
                                  | `modelindex -> (`modelindex,{ai_const=true;ai_ref=true})
                                  | x -> wrap_typ_simple x )
  in
  let (args,res) = typ' |> List.list_last in
  let hndl = FilesMap.find (classname, FilesKey.CHDR) !files in
  fprintfn hndl "public:";
  fprintfn hndl "  Q_INVOKABLE %s %s(%s)%s%s;"
           (cpptyp_of_typ @@ unconst @@ unref res)
           methname
           (List.to_string ~f:(fun _ -> cpptyp_of_typ) args)
           (if minfo.mi_const then " const" else "")
           (if minfo.mi_virt then " override" else "");

  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
  let options = if List.mem `ItemModel ~set:options then [`ItemModel] else [] in
  gen_meth_cpp ~minfo ~options ~classname ~methname hndl typ';
  ()

let itemmodel_externals ~classname =
  [ ("dataChanged", sprintf "caml_%s_dataChanged_cppmeth_wrapper" classname,
     [ `cppobj; `modelindex; `modelindex; `unit])
  ; ("beginInsertRows", sprintf "caml_%s_beginInsertRows_cppmeth_wrapper" classname,
     [ `cppobj; `modelindex; `int; `int; `unit ])
  ; ("endInsertRows", sprintf "caml_%s_endInsertRows_cppmeth_wrapper" classname,
     [ `cppobj; `unit ])
  ; ("beginRemoveRows", sprintf "caml_%s_beginRemoveRows_cppmeth_wrapper" classname,
     [ `cppobj; `modelindex; `int; `int; `unit ])
  ; ("endRemoveRows", sprintf "caml_%s_endRemoveRows_cppmeth_wrapper" classname,
     [ `cppobj; `unit ])
  ; ("addRole", sprintf "caml_%s_addRole_cppmeth_wrapper" classname,
     [ `cppobj; `int; `bytearray; `unit ])
  ]

let itemmodel_members =
  let mi = {mi_virt=true;mi_const=true} in
  [ ("parent",     [`modelindex; `modelindex],                 mi)
  ; ("index",      [`int;`int;`modelindex;`modelindex], mi)
  ; ("columnCount",[`modelindex; `int], mi)
  ; ("rowCount",   [`modelindex; `int], mi)
  ; ("hasChildren",[`modelindex; `bool], mi)
  ; ("data",       [`modelindex; `int;`variant], mi)
  ; ("addRole",    [`string;`int;`unit], {mi_virt=false; mi_const=false} )
  ]


let gen_itemmodel_stuff ~classname =
  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
  let rem_cppobj : _ -> meth_typ_item = function
    | `cppobj -> assert false
    | `unit -> `unit
    | `int  -> `int
    | `bytearray -> `bytearray
    | `string -> `string
    | `modelindex -> `modelindex
    | `variant -> `variant
    | `list x -> `list x
  in
  let f (methname, stubname, types) =
    let types = List.tl types |> List.map ~f:(fun x -> wrap_typ_simple @@ rem_cppobj x) in
    gen_stub_cpp ~options:[`ItemModel] ~classname ~stubname ~methname hndl types
  in
  List.iter ~f (itemmodel_externals ~classname);
  ()
