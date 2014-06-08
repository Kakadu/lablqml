open Printf

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


let gen_prop ~classname ~propname typ =
  printf "Generation prop '%s' of class '%s'.\n" propname classname;
  ()

let gen_meth ~classname ~methname typ =
  printf "Generation meth '%s' of class '%s'.\n" methname classname;
  ()

