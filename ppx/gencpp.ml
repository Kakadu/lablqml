open Stdio
open Base
open Printf


module Time = struct
  let now () = Unix.(localtime @@ time() )
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  let str_of_month n =
    if n>=0 && n<=12 then months.(n)
    else failwith "Wrong argument of str_of_month"
  let to_string {Unix.tm_sec; Unix.tm_mon; Unix.tm_min; Unix.tm_hour; Unix.tm_mday; Unix.tm_year; _ } =
    sprintf "%02d %s, %d %02d:%02d:%02d"
            tm_mday (str_of_month tm_mon) (1900+tm_year) tm_hour tm_min tm_sec
end


(*let printfn fmt = ksprintf (Caml.Format.printf "%s\n") fmt*)
let fprintfn ch fmt = ksprintf (Stdio.Out_channel.fprintf ch "%s\n%!") fmt

let print_time ch =
  fprintfn ch "/*";
  fprintfn ch " * Generated at %s" Time.(now () |> to_string);
  fprintfn ch " */"


let ref_append ~set x = set := x :: !set

module Ref = struct
  include Ref
  let append = ref_append
  let replace x f = (x := f !x)
end

type opt_item = OInstantiable | OItemModel | OItemModelVal of string option

module Options = struct
  type item = opt_item
  type t = item list
(*    [@@deriving show]*)
  let myfind x ~set = List.mem set x ~equal:Stdlib.(=)
  let is_itemmodel set = myfind OItemModel ~set
  let has_itemmodel_val set =
    List.find_map set ~f:(function OItemModelVal x -> Some x | _ -> None)

end



module FilesKey = struct
  type ext = CSRC | CHDR
  type t = string * ext
  let cmp_string : string -> string -> int = String.compare
  let compare a b = match a,b with
    | (_,CSRC),(_,CHDR) -> -1
    | (_,CHDR),(_,CSRC) ->  1
    | (x,_),(y,_) -> cmp_string x y
end
module FilesMap = Stdlib.Map.Make(FilesKey)

let files = ref FilesMap.empty

let open_files ?(destdir=".") ?(ext="cpp") ~options ~classname =
  (*print_endline "Opening files....";*)
  let src = Stdio.Out_channel.create (sprintf "%s/%s_c.%s" destdir classname ext) in
  let hdr = Stdio.Out_channel.create (sprintf "%s/%s.h" destdir classname) in
  print_time hdr;
  let println fmt = fprintfn hdr fmt in
  println "#ifndef %s_H" (Stdlib.String.uppercase_ascii classname);
  println "#define %s_H" (Stdlib.String.uppercase_ascii classname);
  println "";
  println "#include <QtCore/QDebug>";
  println "#include <QtCore/QObject>";
  println "#include <QtCore/QAbstractItemModel>";
  println "";
  println "#ifdef __cplusplus";
  println "extern \"C\" {";
  println "#endif";
  println "#include <caml/alloc.h>";
  println "#include <caml/mlvalues.h>";
  println "#include <caml/callback.h>";
  println "#include <caml/memory.h>";
  println "#include <caml/threads.h>";
  println "#ifdef __cplusplus";
  println "}";
  println "#endif";
  println "";
  println "class %s : public %s {" classname
           (if Options.is_itemmodel options then "QAbstractItemModel" else "QObject");
  println "  Q_OBJECT";
  println "  value _camlobjHolder; // store reference to OCaml value there";
  println "public:";
  println "  %s() : _camlobjHolder(0) { };" classname;
  println "  void storeCAMLobj(value x) {";
  println "    if (_camlobjHolder != 0) {";
  println "       //maybe unregister global root?";
  println "    }";
  println "    _camlobjHolder = x;";
  println "    register_global_root(&_camlobjHolder);";
  println "  }\n";
  let () =
    if Options.is_itemmodel options then (
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
  if PpxQtCfg.config.insert_locks
  then
    let () =
      if PpxQtCfg.config.trace_locks
      then fprintfn ch "  qDebug() << \"release_runtime_system();\";"
    in
    fprintfn ch "  caml_release_runtime_system();"


let leave_blocking_section ch =
  if PpxQtCfg.config.insert_locks
  then
    let () =
      if PpxQtCfg.config.trace_locks
      then fprintfn ch "  qDebug() << \"acquire_runtime_system();\";"
    in
    fprintfn ch "  caml_acquire_runtime_system();"



let close_files ~options:_ =
  (*print_endline "Closing files";*)
  let f (classname,ext) hndl =
    let println fmt = fprintfn hndl fmt in
    match ext with
    | FilesKey.CHDR ->
       println "};";
       println "#endif /* %s_H */\n" (Stdlib.String.uppercase_ascii classname);
       println "extern \"C\" value caml_create_%s(value _dummyUnitVal);" classname;
       println "extern \"C\" value caml_store_value_in_%s(value _cppobj,value _camlobj);" classname;
       (*
       if List.mem `Instantiable options
       then
         println "extern \"C\" value caml_register_%s(value _ns,value _major,value _minor,value _classname,value _constructor);" classname;
       *)
       Stdio.Out_channel.flush hndl;
       Stdio.Out_channel.close hndl
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
(*
        println "extern \"C\" value caml_register_%s(value _ns,value _major,value _minor,value _classname,value _constructor) {" classname;
        println "  CAMLparam5(_ns, _major, _minor, _classname, value _constructor);";
        enter_blocking_section hndl;
        println "  int major = Int_val(_major);";
        println "  int minor = Int_val(_minor);";
        println "  QString ns = QString(String_val(_minor));";
        println "  QString cname = QString(String_val(_classname));";
        println "  qmlRegisterType<%s>(ns, major, minor, cname);" classname;
        println "  o->storeCAMLobj(_camlobj);";
        leave_blocking_section hndl;
        println "  CAMLreturn(Val_unit);";
        println "}";*)

        Stdio.Out_channel.flush hndl;
        Stdio.Out_channel.close hndl
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
    Int.incr last_index;
    sprintf "%s%d" prefix !last_index
    in
    f

(* TODO: add addtitional info about methods *)
(* We need this arginfo because void foo(QString) is not the same as void foo(const QString&) *)
type arg_info = { ai_ref: bool; ai_const: bool }
type meth_info = { mi_virt: bool; mi_const: bool }

let mi_empty = { mi_virt=false; mi_const=false }
let ai_empty = { ai_ref=false; ai_const=false }
let wrap_typ_simple x = (x, ai_empty)
let unref   (x, y) = (x, { y with ai_ref=false })
let unconst (x, y) = (x, { y with ai_const=false })

module Arg : sig
  type default = [ `Default ]
  type model = [ `Model ]
  type cppobj = [ `Cppobj ]
  type non_cppobj = [ default | model ]
  type any = [ cppobj | non_cppobj ]

  type _ t =
    | Unit          : [> `Default ] t
    | QString       : [> `Default ] t
    | Int           : [> `Default ] t
    | Bool          : [> `Default ] t
    | QVariant      : [> `Default ] t
    | QByteArray    : [> `Default ] t
    | QList         : 'a t -> 'a t
    | QModelIndex   : [> `Model ] t
    | Cppobj        : [> `Cppobj ] t


  val default_plus_model : default t -> [ default | model ] t
  val model_plus_default : model t -> [ default | model ] t
  val remove_cppobj : any t -> non_cppobj t option
end = struct
  type default = [ `Default ]
  type model   = [ `Model ]
  type cppobj  = [ `Cppobj ]
  type non_cppobj = [ default | model ]
  type any = [ cppobj | default | model ]

  type _ t =
    | Unit          : [> `Default ] t
    | QString       : [> `Default ] t
    | Int           : [> `Default ] t
    | Bool          : [> `Default ] t
    | QVariant      : [> `Default ] t
    | QByteArray    : [> `Default ] t
    | QList         : 'a t -> 'a t
    | QModelIndex   : [> `Model ] t
    | Cppobj        : [> `Cppobj ] t

  let rec model_plus_default : model t -> [ default | model ] t = function
    | QModelIndex as y -> y
    | QList x -> QList (model_plus_default x)

  let rec cppobj_to_any : cppobj t -> any t = function
    | Cppobj -> Cppobj
    | QList x -> QList (cppobj_to_any x)

  let rec model_to_any : model t -> any t = function
    | QModelIndex -> QModelIndex
    | QList x -> QList (model_to_any x)

  let rec default_plus_model : default t -> [ default | model ] t = function
    | (Unit as y)
    | (QString as y)
    | (Int as y)
    | (Bool as y)
    | (QVariant as y)
    | (QByteArray as y) -> y
    | QList x -> QList (default_plus_model x)

  let rec remove_cppobj : any t -> non_cppobj t option = function
    | Cppobj -> None
    | Unit -> Some Unit
    | Int    -> Some Int
    | Bool   -> Some Bool
    | QByteArray  -> Some QByteArray
    | QString     -> Some QString
    | QModelIndex -> Some QModelIndex
    | QVariant    -> Some QVariant
    | QList xs    ->
        match remove_cppobj xs with
        | None -> None
        | Some y -> Some (QList y)
end[@warning "-32"]

open Arg

(* how many additional variables needed to convert C++ value to OCaml one *)
let aux_variables_count  =
  let rec helper : non_cppobj Arg.t -> int = function
    | QByteArray
    | Bool | Int | Unit | QString | QModelIndex -> 0
    | QList x -> helper x + 2
    | QVariant -> 2
  in
  helper

(* how many additional variables needed to convert OCaml value to C++ one *)
let aux_variables_count_to_cpp  =
  let rec helper : non_cppobj Arg.t -> int = function
    | QVariant
    | QByteArray
    | Bool | Int | Unit | QString | QModelIndex -> 0
    | QList x -> helper x + 2
  in
  helper

let rec ocaml_ast_of_typ : any Arg.t -> Longident.t = fun x ->
  let open Longident in
  match x with
  | Cppobj     -> Lident "cppobj"
  | QVariant   -> Ldot (Lident "QVariant",   "t")
  | QModelIndex-> Ldot (Lident "QModelIndex","t")
  | Bool       -> Lident "bool"
  | Unit       -> Lident "unit"
  | QByteArray
  | QString    -> Lident "string"
  | Int        -> Lident "int"
  | QList x    -> Lapply (Lident "list", ocaml_ast_of_typ x)

let cpptyp_of_typ =
  let rec helper: non_cppobj Arg.t * arg_info -> string = fun (x,ai) ->
    match x with
    | Bool -> "bool"
    | Int  -> "int"
    | QVariant -> "QVariant"
    | QString    -> "QString"
    | QByteArray -> "QByteArray"
    | Unit    -> "void"
    | QModelIndex -> sprintf "%sQModelIndex%s" (if ai.ai_const then "const " else "")
                             (if ai.ai_ref then "&" else "")
    | QList x -> sprintf "%sQList<%s>%s" (if ai.ai_const then "const " else "")
                         (helper (x, ai_empty))
                         (if ai.ai_ref then "&" else "")
  in
  helper

let rec cpptyp_of_proptyp: default Arg.t * arg_info -> string = fun (typ,ai) ->
  let upcasted = (default_plus_model typ, ai) in
  let cppname =
    match typ with
    | Bool -> cpptyp_of_typ upcasted
    | Int  -> cpptyp_of_typ upcasted
    | QVariant -> cpptyp_of_typ upcasted
    | QByteArray -> cpptyp_of_typ upcasted
    | QString    -> cpptyp_of_typ upcasted

    | Unit -> failwith "should not happen"
    | QList x ->
        sprintf "QList<%s>"
        (cpptyp_of_proptyp (x, ai_empty))
  in
  sprintf "%s%s%s"
    (if ai.ai_const then "const " else "")
    cppname
    (if ai.ai_ref then "&" else "")

type mode = Local | Param
let print_declarations ?(mode=Local) ch xs =
  let m = match mode with Local -> "CAMLlocal" | Param -> "CAMLparam" in
  let rec helper = function
  | a::b::c::d::e::xs ->
     Stdio.Out_channel.fprintf ch "  %s5(%s);\n" m (String.concat ~sep:"," [a;b;c;d;e]);
     helper xs
  | [] -> ()
  | xs ->
     let n = List.length xs in
     assert (n<5);
     Stdio.Out_channel.fprintf ch "  %s%d(%s);\n" m n (String.concat ~sep:"," xs)
  in
  helper xs

let print_local_declarations ch xs = print_declarations ~mode:Local ch xs
let print_param_declarations ch xs = print_declarations ~mode:Param ch xs

let cpp_value_of_ocaml
      ?(options=[]) ~cppvar ~ocamlvar
      ch (get_var,release_var,new_cpp_var) : non_cppobj Arg.t -> unit =

  let rec helper ~tab dest ~ocamlvar : non_cppobj Arg.t -> unit = fun typ ->
    let prefix = String.make (2*tab) ' ' in
    let println fmt = Stdio.Out_channel.fprintf ch "%s" prefix; fprintfn ch fmt in
    match typ with
    | Unit       -> ()
    | Int        -> println "%s = Int_val(%s);" dest ocamlvar
    | QString    -> println "%s = QString(String_val(%s));" dest ocamlvar
    | QByteArray -> println "%s = QByteArray(String_val(%s));" dest ocamlvar
    | Bool       -> println "%s = Bool_val(%s);" dest ocamlvar
    | QModelIndex ->
       begin
         match Options.has_itemmodel_val  options with
         | Some obj ->
            let call =
              match obj with
              | Some o -> sprintf "%s->makeIndex" o
              | None  -> "createIndex"
            in
            println "%s = %s(Int_val(Field(%s,0)), Int_val(Field(%s,1)) );"
                    dest call ocamlvar ocamlvar
         | None -> failwith "QModelIndex is not available without QAbstractItemModel base"
       end
    | QVariant ->
       println "if (Is_block(%s)) {" ocamlvar;
       println "  if (caml_hash_variant(\"string\") == Field(%s,0))" ocamlvar;
       println "    %s = QVariant::fromValue(QString(String_val(Field(%s,1))));" dest ocamlvar;
       println "  else if(caml_hash_variant(\"int\") == Field(%s,0))" ocamlvar;
       println "    %s = QVariant::fromValue(Int_val(Field(%s,1)));" dest ocamlvar;
       println "  else if(caml_hash_variant(\"bool\") == Field(%s,0))" ocamlvar;
       println "    %s = QVariant::fromValue(Bool_val(Field(%s,1)));" dest ocamlvar;
       println "  else if(caml_hash_variant(\"float\") == Field(%s,0))" ocamlvar;
       println "    %s = QVariant::fromValue(Double_val(Field(%s,1)));" dest ocamlvar;
       println "  else if(caml_hash_variant(\"qobject\") == Field(%s,0))" ocamlvar;
       println "    %s = QVariant::fromValue((QObject*) (Field(Field(%s,1),0)));" dest ocamlvar;
       println "  else Q_ASSERT_X(false,\"%s\",\"%s\");"
                 "While converting OCaml value to QVariant"
                 "Unknown variant tag";
       println "} else { // empty QVariant";
       println "    %s = QVariant();" dest;
       println "}"
    | QList t ->
       let cpp_typ_str = cpptyp_of_typ @@ wrap_typ_simple typ  in
       let cpp_argtyp_str = cpptyp_of_typ @@ wrap_typ_simple t in
       println "// generating %s" cpp_typ_str;
       let temp_var = get_var () in
       let head_var = get_var () in
       let temp_cpp_var = new_cpp_var () in
       println "%s = %s;\n" temp_var ocamlvar;
       println "while (%s != Val_emptylist) {\n" temp_var;
       println "  %s = Field(%s,0); /* head */"  head_var temp_var;
       println "  %s %s;" cpp_argtyp_str temp_cpp_var;
       helper  ~tab:(tab+1) temp_cpp_var ~ocamlvar:head_var t;
       println "  %s << %s;\n" dest temp_cpp_var;
       println "  %s = Field(%s,1);" temp_var temp_var;
       println "}";
       release_var head_var;
       release_var temp_var
  in
  helper ~tab:1 cppvar ~ocamlvar

let ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar
       : non_cppobj Arg.t -> unit =

  let rec helper ~tab ~var ~dest (typ : non_cppobj Arg.t) =
    let println fmt = Out_channel.fprintf ch "%s" (String.make (2*tab) ' '); fprintfn ch fmt in
    match typ with
    | Unit       -> failwith "Can't generate OCaml value from C++ void a.k.a. unit"
    | Int        -> println "%s = Val_int(%s);" dest var
    | Bool       -> println "%s = Val_bool(%s);" dest var
    | QString     -> println "%s = caml_copy_string(%s.toLocal8Bit().data());" dest var
    | QByteArray  -> println "%s = caml_copy_string(%s.toLocal8Bit().data());" dest var
    | QModelIndex ->
       println "%s = caml_alloc(2,0);" dest;
       println "Store_field(%s,0,Val_int(%s.row()));" dest var;
       println "Store_field(%s,1,Val_int(%s.column()));" dest var
    | QVariant ->
       println "if (!%s.isValid())" var;
       println "  %s=hash_variant(\"empty\");" dest;
       println "else {";
       println "  int ut = %s.userType();" var;
       println "  if(ut == QMetaType::QString) {";
       println "    %s = caml_alloc(2,0);" dest;
       println "    Store_field(%s,0,%s);" dest "hash_variant(\"string\")";
       println "    Store_field(%s,1,%s);" dest
                 (sprintf "caml_copy_string(%s.value<QString>().toLocal8Bit().data())" var);
       println "  } else if (ut == QMetaType::Int) { ";
       println "    %s = caml_alloc(2,0);" dest;
       println "    Store_field(%s, 0, %s);" dest "hash_variant(\"int\")";
       println "    Store_field(%s, 1, Val_int(%s.value<int>()));" dest var;
       println "  } else if (ut == QMetaType::Double) { ";
       println "    %s = caml_alloc(2,0);" dest;
       println "    Store_field(%s, 0, %s);" dest "hash_variant(\"float\")";
       println "    Store_field(%s, 1, caml_copy_double(%s.value<double>()) );" dest var;
       println "  } else if (ut == QMetaType::Bool) { ";
       println "    %s = caml_alloc(2,0);" dest;
       println "    Store_field(%s, 0, %s);" dest "hash_variant(\"bool\")";
       println "    Store_field(%s, 1, Val_bool(%s.value<bool>()));" dest var;
       println "  } else if((ut==QMetaType::User) ||";
       println "            (ut==QMetaType::QObjectStar)) {"; (*custom QObject*)
       println "    QObject *vvv = %s.value<QObject*>();" var;
       let objvar = get_var() in
       println "    %s = caml_alloc_small(1,Abstract_tag);" objvar;
       println "    (*((QObject **) &Field(%s, 0))) = vvv;" objvar;
       println "    %s = caml_alloc(2,0);" dest;
       println "    Store_field(%s, 0, hash_variant(\"qobject\"));" dest;
       println "    Store_field(%s, 1, %s);" dest objvar;
       println "  } else {";
       println "    QString msg(\"Type is not supported:\");";
       println "    msg += QString(\"userType() == %%1\").arg(ut);";
       println "    Q_ASSERT_X(false,\"qVariant_of_cpp\", msg.toLocal8Bit().data() );";
       println "  }";
       println "}"

    | QList t ->
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
  helper ~tab:1 ~var:cppvar ~dest:ocamlvar

(* stub implementation to call it from OCaml *)
let gen_stub_cpp ?(options=[]) ~classname ~stubname ~methname ch
                 (types: (non_cppobj Arg.t * arg_info) list) =
  let println fmt = fprintfn ch fmt in
  let (args,res) = List.(drop_last_exn types, last_exn types) in
  let res = unref res in
  println "// stub: %s name(%s)" (cpptyp_of_typ res) (List.map ~f:cpptyp_of_typ args |> String.concat ~sep:",");
  let argnames = List.mapi ~f:(fun i _ -> sprintf "_x%d" i) args in
  let cppvars  = List.mapi ~f:(fun i _ -> sprintf "z%d" i) args in
  println "extern \"C\" value %s(%s) {"
          stubname
          (List.map ~f:(sprintf "value %s") ("_cppobj"::argnames) |> String.concat ~sep:",");
  print_param_declarations ch ("_cppobj"::argnames);
  let aux_count =
    List.fold_left ~f:(fun acc x -> max (aux_variables_count_to_cpp @@ fst x) acc) ~init:0 args
  in
  let aux_count = max aux_count (aux_variables_count @@ fst res) in
  println "  // aux vars count = %d" aux_count;
  let local_names = List.init ~f:(sprintf "_x%d") aux_count in
  print_local_declarations ch local_names;

  enter_blocking_section ch;
  println "  %s *o = (%s*) (Field(_cppobj,0));" classname classname;

  let get_var,release_var = get_vars_queue local_names in
  let cpp_var_counter = ref 0 in
  let new_cpp_var () = Int.incr cpp_var_counter; sprintf "zz%d" !cpp_var_counter in

  let options = if Options.is_itemmodel options then [OItemModelVal (Some "o")] else [] in
  let f = fun i arg ->
    let cppvar = sprintf "z%d" i in
    let ocamlvar = sprintf "_x%d" i in
    println "  %s %s;" (cpptyp_of_typ arg) cppvar;
    cpp_value_of_ocaml ch ~options ~cppvar ~ocamlvar (get_var,release_var,new_cpp_var) (fst arg)
  in
  List.iteri ~f args;
  let () = match res with
    | (Unit,_) ->
       println "  o->%s(%s);" methname (String.concat ~sep:"," cppvars);
       leave_blocking_section ch;
       println "  CAMLreturn(Val_unit);";
    | (_t, _ai)   ->
       let cppvar = "res" in
       println "  %s %s = o->%s(%s);" (cpptyp_of_typ res) cppvar methname (String.concat ~sep:"," cppvars);
       let ocamlvar = "_ans" in
       Out_channel.fprintf ch "  ";
       ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar (fst res);
       leave_blocking_section ch;
       println "  CAMLreturn(%s);" ocamlvar
  in
  println "}";
  ()

(* method implementation in C++ file. Used for invocation OCaml from C++ *)
let gen_meth_cpp ~minfo ?(options=[]) ~classname ~methname ch types =
  let println fmt = fprintfn ch fmt in
  let print   fmt = Out_channel.fprintf  ch fmt in
  println "// %s::%s: %s" classname methname
    (List.map ~f:cpptyp_of_typ types |> String.concat ~sep:",");
  let (args,res) = List.(drop_last_exn types, last_exn types) in
  let res = unconst @@ unref res in
  let () =
    match fst res with
    | Unit -> print "void "
    | _     -> print "%s " (cpptyp_of_typ res)
  in
  println "%s::%s(%s) %s {" classname methname
          (match args with
           | [(Unit,_)] -> ""
           | _ ->
              String.concat ~sep:"," @@
              List.mapi ~f:(fun i t -> sprintf "%s x%d" (cpptyp_of_typ t) i) args)
          (if minfo.mi_const then " const" else "");
  println "  CAMLparam0();";
  let locals_count = 2 +
    List.fold_left ~f:(fun acc (x,_) -> max acc (aux_variables_count x)) ~init:0 types
  in
  let locals = List.init ~f:(sprintf "_x%d") (locals_count-1) in
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
    | 1 when (match args with [Unit,_] -> true | _ -> false) ->
       sprintf "caml_callback2(_meth, _camlobj, Val_unit);"
    | n ->
       println "  _args[0] = _camlobj;";
       List.iteri args ~f:(fun i (typ,_) ->
         let cppvar = sprintf "x%d" i in
         let ocamlvar = make_cb_var i in
         print "  ";
         ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar typ;
         println "  _args[%d] = %s;" (i+1) ocamlvar
       );
       sprintf "caml_callbackN(_meth, %d, _args);" (n+1)
  in
  let () =
    match fst res with
    | Unit  ->
      println "  %s" call_closure_str;
      enter_blocking_section ch;
      println "  CAMLreturn0;"
    | _ ->
      let options = [ OItemModelVal (Some "this") ] in
      let ocamlvar = "_ans" in
      let cpp_res_typ = cpptyp_of_typ res in
      println "  %s = %s;" ocamlvar call_closure_str;
      enter_blocking_section ch;
      let cppvar = "cppans" in
      println "  %s %s;" cpp_res_typ cppvar;
      let new_cpp_var = getter_of_cppvars "xx" in
      cpp_value_of_ocaml ~options ~cppvar ~ocamlvar ch (get_var,release_var, new_cpp_var) (fst res);
      println "  CAMLreturnT(%s,%s);" cpp_res_typ cppvar;
  in
  println "}";
  ()

let gen_prop ~classname ~propname (typ: default Arg.t) =
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
               [ (default_plus_model Unit, ai_empty)
               ; (default_plus_model typ , ai_empty)
               ];
  let stubname = sprintf "caml_%s_%s_cppmeth_wrapper" classname sgnl_name in
  gen_stub_cpp ~classname ~methname:sgnl_name ~stubname
               hndl
               [ (default_plus_model typ , ai_empty)
               ; (default_plus_model Unit, ai_empty)
               ];
  ()

let gen_signal ~classname ~signalname types' =
  (* args are sent without last unit *)
  let hndl = FilesMap.find (classname, FilesKey.CHDR) !files in
  let types = List.map types' ~f:snd in
  let argnames = List.map types' ~f:fst in

  (*let () = List.iter ~f:(fun x -> assert(x<>"")) argnames in *)
  (*let () = List.iter ~f:(fun x -> print_endline x) argnames in*)

  let println fmt = fprintfn hndl fmt in
  println "signals:";
  let types = List.map types ~f:wrap_typ_simple in
  let f t name = sprintf "%s %s" (cpptyp_of_typ t) name in
  println "  void %s(%s);" signalname (List.map2_exn ~f types argnames |> String.concat);

  let stubname: string  = sprintf "caml_%s_%s_emitter_wrapper" classname signalname in
  let hndl = FilesMap.find (classname, FilesKey.CSRC) !files in
  gen_stub_cpp ~options:[] ~classname ~methname:signalname ~stubname
               hndl
               (types @ [(default_plus_model Unit, ai_empty)] );
  ()


let gen_meth ?(minfo=mi_empty) ?(options=[]) ~classname ~methname typs =
  let (_: non_cppobj t list) = typs in
  (*printf "Generation meth '%s' of class '%s'.\n" methname classname;*)

  let (typ', args, res) =
    let ts = List.map typs
      ~f:(function
         | QModelIndex -> (model_plus_default Arg.QModelIndex  , {ai_const=true;ai_ref=true})
         | x -> wrap_typ_simple x )
    in
    (ts, List.drop_last_exn ts, List.last_exn ts)
  in
  let hndl = FilesMap.find (classname, FilesKey.CHDR) !files in
  fprintfn hndl "public:";
  fprintfn hndl "  Q_INVOKABLE %s %s(%s)%s%s;"
           (cpptyp_of_typ @@ unconst @@ unref res)
           methname
           (List.mapi ~f:(fun _ -> cpptyp_of_typ) args |> String.concat ~sep:",")
           (if minfo.mi_const then " const" else "")
           (if minfo.mi_virt then " override" else "");

  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
(*  let options = if Options.is_itemmodel options then [OItemModel] else [] in*)
  gen_meth_cpp ~minfo ~options ~classname ~methname hndl typ';
  ()

let itemmodel_externals ~classname :
  (string * string * any Arg.t list) list =
  [ ("dataChanged", sprintf "caml_%s_dataChanged_cppmeth_wrapper" classname,
     [ Cppobj
     ; QModelIndex
     ; QModelIndex
     ; Unit
     ])
  ; ("beginInsertRows", sprintf "caml_%s_beginInsertRows_cppmeth_wrapper" classname,
     [ Cppobj
     ; QModelIndex
     ; Int
     ; Int
     ; Unit
     ])
  ; ("endInsertRows", sprintf "caml_%s_endInsertRows_cppmeth_wrapper" classname,
     [ Cppobj
     ; Unit
     ])
  ; ("beginRemoveRows", sprintf "caml_%s_beginRemoveRows_cppmeth_wrapper" classname,
     [ Cppobj
     ; QModelIndex
     ; Int
     ; Int
     ; Unit
     ])
  ; ("endRemoveRows", sprintf "caml_%s_endRemoveRows_cppmeth_wrapper" classname,
     [ Cppobj
     ; Unit
     ])
  ; ("addRole", sprintf "caml_%s_addRole_cppmeth_wrapper" classname,
     [ Cppobj
     ; Int
     ; QByteArray
     ; Unit
     ])
  ]

let itemmodel_members : (_ * non_cppobj Arg.t list * _) list =
  let mi = { mi_virt=true; mi_const=true } in
  let wrap ?(i=mi) name xs = (name, xs, i) in

  [ wrap "parent"
      [ QModelIndex
      ; QModelIndex
      ]
  ; wrap "index"
      [ Int
      ; Int
      ; QModelIndex
      ; QModelIndex ]
  ; wrap "columnCount"
      [ QModelIndex
      ; Int
      ]
  ; wrap "rowCount"
      [ QModelIndex
      ; Int
      ]
  ; wrap "hasChildren"
      [ QModelIndex
      ; Bool
      ]
  ; wrap "data"
      [ QModelIndex
      ; Int
      ; QVariant
      ]
  ; wrap "addRole" ~i:{mi_virt=false; mi_const=false}
      [ QString
      ; Int
      ; Unit
      ]
  ]

let gen_itemmodel_stuff ~classname =
  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
  let f (methname, stubname, types) =
    (* first type is for this in OCaml *)
    let types = List.tl_exn types in
    let types = List.filter_map ~f:(fun x -> remove_cppobj x) types in
    let types = List.map ~f:wrap_typ_simple types in
    gen_stub_cpp ~options:[OItemModel] ~classname ~stubname ~methname hndl types
  in
  List.iter ~f (itemmodel_externals ~classname);
  ()
