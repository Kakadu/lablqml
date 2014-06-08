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
end

module Time = struct
  let now () = Unix.(localtime @@ time() )
  let months = [| "Jan"; "Feb"; "Mar"; "Apr"; "May"; "Jun"; "Jul"; "Aug"; "Sep"; "Oct"; "Nov"; "Dec" |]
  let str_of_month n =
    if n>=0 && n<=12 then months.(n)
    else failwith "Wrong argument of str_of_month"
  let to_string {Unix.tm_sec; Unix.tm_mon; Unix.tm_min; Unix.tm_hour; Unix.tm_mday; Unix.tm_year } =
    sprintf "%02d %s, %d %d:%d:%d" tm_mday (str_of_month tm_mon) (1900+tm_year) tm_hour tm_min tm_sec
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

let open_files ?(destdir=".") ~classname =
  let src = open_out (sprintf "%s/%s.cpp" destdir classname) in
  let hdr = open_out (sprintf "%s/%s.h" destdir classname) in
  print_time hdr;
  fprintfn hdr "#ifndef %s_H" (String.uppercase classname);
  fprintfn hdr "#define %s_H" (String.uppercase classname);
  fprintfn hdr "";
  fprintfn hdr "#include <QtCore/QDebug>";
  fprintfn hdr "#include <QtCore/QObject>";
  fprintfn hdr "#include <caml/alloc.h>";
  fprintfn hdr "#include <caml/mlvalues.h>";
  fprintfn hdr "#include <caml/callback.h>";
  fprintfn hdr "#include <caml/memory.h>";
  fprintfn hdr "#include <caml/threads.h>";
  fprintfn hdr "";
  fprintfn hdr "class %s : public QObject {" classname;
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
  files := FilesMap.(add (classname, FilesKey.CHDR) hdr !files);
  print_time src;
  fprintfn src "#include \"%s.h\"" classname;
  fprintfn src "";
  files := FilesMap.(add (classname, FilesKey.CSRC) src !files)

let enter_blocking_section ch =
  fprintfn ch "  enter_blocking_section();"

let leave_blocking_section ch =
  fprintfn ch "  leave_blocking_section();"

let close_files () =
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
  let getter_of_prop s = s
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

(* properties can have only simple types (except unit) *)
type prop_typ = [ `bool | `int | `string | `list of prop_typ ]
type meth_typ_item = [ `unit | `bool | `int | `string | `list of meth_typ_item ]
type meth_typ = meth_typ_item list

let aux_variables_count (y: meth_typ_item) =
  let rec helper = function
    | `bool | `int | `unit | `string -> 0
    | `list x -> helper x + 2
  in
  helper y

let rec cpptyp_of_proptyp: prop_typ -> string = function
  | `bool -> "bool"
  | `int  -> "int"
  | `string -> "QString"
  | `list x -> sprintf "QList<%s>" (cpptyp_of_proptyp x)

let rec cpptyp_of_typ: meth_typ_item -> string = fun x -> match x with
  | `bool  | `int  | `string as x -> cpptyp_of_proptyp x
  | `unit -> "void"
  | `list x -> sprintf "QList<%s>" (cpptyp_of_typ x)

let print_local_declarations ch xs =
  let rec helper = function
  | a::b::c::d::e::xs ->
     fprintf ch "  CAMLlocal5(%s);\n" (String.concat "," [a;b;c;d;e]);
     helper xs
  | [] -> ()
  | xs ->
     let n = List.length xs in
     assert (n<5);
     fprintf ch "  CAMLlocal%d(%s);\n" n (String.concat "," xs)
  in
  helper xs

let cpp_value_of_ocaml ~cppvar ~ocamlvar ch (get_var,release_var,new_cpp_var) typ =
  let rec helper ~tab dest var typ =
    let prefix = String.make (2*tab) ' ' in
    let println fmt = fprintf ch "%s" prefix; fprintfn ch fmt in
    match typ with
    | `unit  -> ()
    | `int   -> println "%s = Int_val(%s);" cppvar ocamlvar
    | `string-> println "%s = QString(String_val(%s));" cppvar ocamlvar
    | `bool  -> println "%s = Bool_val(%s);" cppvar ocamlvar
    | `list t->
       let cpp_typ_str = cpptyp_of_typ typ in
       println "// generating %s" cpp_typ_str;
       let temp_var = get_var () in
       let head_var = get_var () in
       let temp_cpp_var = new_cpp_var () in
       println "%s = %s;\n" temp_var var;
       println "while (%s != Val_emptylist) {\n" temp_var;
       println "  %s = Field(%s,0); /* head */"  head_var temp_var;
       println "  %s %s;" cpp_typ_str temp_cpp_var;
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
    | `unit -> failwith "Can't generate OCaml value from C++ void a.k.a. unit"
    | `int  -> println "%s = Val_int(%s);" dest var
    | `bool -> println "%s = Val_bool(%s);" dest var
    | `string -> println "%s = caml_copy_string(%s.toLoca8Bit().data()));" dest var
    | `list t ->
       let cons_helper = get_var () in
       let cons_arg_var = get_var () in
       println "%s = Val_emptylist;\n" dest;
       println "if ((%s).length() != 0) {" var;
       println "  auto it = (%s).end() - 1;" var;
       println "  for (;;) {";
       println "    %s = caml_alloc(2,0);" cons_helper;
       helper ~tab:(tab+1) ~var:"(*it)" ~dest:cons_arg_var typ;
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
let gen_stub_cpp ~classname ~methname ch (types: meth_typ) =
  fprintfn ch "// stub"

(* method implementation from class header. Used for invacation OCaml from C++ *)
let gen_meth_cpp ~classname ~methname ch (types: meth_typ) =
  let println fmt = fprintfn ch fmt in
  let print   fmt = fprintf  ch fmt in
  fprintfn ch "// meth";
  let (args,res) = List.list_last types in
  if res=`unit
  then print "void "
  else print "%s " (cpptyp_of_typ res);

  println "%s::%s(%s) {" classname methname
          (if args=[`unit] then ""
           else List.to_string ~f:(fun i t -> sprintf "%s x%d" (cpptyp_of_typ t) i) args);
  println "  CAMLparam0();";
  let locals_count = 2 +
    List.fold_left ~f:(fun acc x -> max acc (aux_variables_count x)) ~init:0 types in
  let locals = List.init (sprintf "_l%d") (locals_count-1) in
  print_local_declarations ch ("_ans" :: "_meth" :: locals);
  println "  // aux vars count = %d" locals_count;
  let make_cb_var = sprintf "_cca%d" in (* generate name *)
  let cb_locals = List.mapi ~f:(fun i _ -> make_cb_var i) args in
  print_local_declarations ch cb_locals;
  (* array for invocing OCaml method *)
  println "  CAMLlocalN(_args, %d);" (List.length args + 1);
  leave_blocking_section ch;

  println "  value _camlobj = this->_camlobjHolder;";
  println "  Q_ASSERT(Is_block(_camlobj));";
  println "  Q_ASSERT(Tag_val(_camlobj) == Object_tag);";
  println "  _meth = caml_get_public_method(_camlobj, caml_hash_variant(\"%s\"));" methname;
  println "";

  let get_var,release_var = get_vars_queue locals in
  let call_closure_str = match List.length args with
    | 0 -> sprintf "caml_callback2(_meth, _camlobj, Val_unit);"
    | n ->
       println "  _args[0] = _camlobj;";
       let f i arg =
         let cppvar = sprintf "x%d" i in
         let ocamlvar = make_cb_var i in
         let name = List.nth cb_locals i in
         fprintf ch "  ";
         ocaml_value_of_cpp ch (get_var,release_var) ~ocamlvar ~cppvar arg;
         println "  _args[%d] = %s;" (i+1) ocamlvar
       in
       List.iteri ~f  args;
       sprintf "  caml_callbackN(_meth, %d, _args);" (n+1)
  in
  if res = `unit then (
    println "  %s" call_closure_str;
    enter_blocking_section ch;
    println "  CAMLreturn0;"
  )else(
    let ocamlvar = "_ans" in
    let cpp_res_typ = cpptyp_of_typ res in
    println "  %s = %s;" ocamlvar call_closure_str;
    enter_blocking_section ch;
    let cppvar = "cppans" in
    println "  %s %s;" cpp_res_typ cppvar;
    let new_cpp_var = getter_of_cppvars "xx" in
    cpp_value_of_ocaml ~cppvar ~ocamlvar ch  (get_var,release_var, new_cpp_var) res;
    println "  CAMLreturnT(%s,%s);" cpp_res_typ cppvar;
  );
  println "}";
  ()

let gen_prop ~classname ~propname (typ: prop_typ) =
  printf "Generation prop '%s' of class '%s'.\n" propname classname;
  let println fmt =
    let hndl = FilesMap.find (classname,FilesKey.CHDR) !files in
    fprintfn hndl fmt
  in
  let sgnl_name = Names.signal_of_prop propname in
  let getter_name = Names.getter_of_prop propname in
  let setter_name = Names.setter_of_prop propname in
  let cpptyp_name = cpptyp_of_proptyp typ in

  println "public:";
  println "  Q_PROPERTY(%s %s READ %s NOTIFY %s)" cpptyp_name propname getter_name sgnl_name;
  println "  %s %s();" cpptyp_name getter_name;
  (* println "  void emit_%s(%s x) { emit %s(x); }" sgnl_name cpptyp_name sgnl_name;*)
  println "signals:";
  println "  void %s(%s %s);" sgnl_name cpptyp_name propname;
  (* C++ part now *)
  let hndl = FilesMap.find (classname,FilesKey.CSRC) !files in
  let println fmt = fprintfn hndl fmt in
  (*println "// Q_PROPERTY( %s )" propname;*)
  gen_meth_cpp ~classname ~methname:getter_name hndl [(`unit :> meth_typ_item); (typ :>meth_typ_item)];
  ()

let gen_meth ~classname ~methname typ =
  printf "Generation meth '%s' of class '%s'.\n" methname classname;
  ()

