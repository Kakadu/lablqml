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
  print_time src;
  fprintfn hdr "#ifndef %s_H" (String.uppercase classname);
  fprintfn hdr "#define %s_H" (String.uppercase classname);
  fprintfn hdr "";
  fprintfn hdr "class %s : public QObject {" classname;
  files := FilesMap.(add (classname, FilesKey.CSRC) src (add (classname, FilesKey.CHDR) hdr !files) )

let close_files () =
  let f (classname,ext) hndl =
    match ext with
    | FilesKey.CHDR ->
       let println fmt = fprintfn hndl fmt in
       println "};";
       println "#endif /* %s_H */\n" (String.uppercase classname);
       close_out hndl
    | FilesKey.CSRC ->
       close_out hndl
  in
  FilesMap.iter f !files;
  files := FilesMap.empty


let gen_prop ~classname ~propname typ =
  printf "Generation prop '%s' of class '%s'.\n" propname classname;
  ()
