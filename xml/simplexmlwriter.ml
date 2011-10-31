open Printf
open Simplexmlparser

module M = Map.Make(struct type t = int let compare = compare end)

exception Bug of string
let bug s = raise (Bug s)

let bad_pcdata s =
  try
    ignore(Str.search_forward (Str.regexp "]]>") s 0 : int);
    true
  with
      Not_found -> false

let string_of_attr (a,b) = Printf.sprintf "%s=\"%s\"" a b

let print ~out ?(tabc=4) root = 
  let prefixes = ref M.empty in
  let prefix i = 
(*    printf "searching prefix %d\n" i; *)
    if M.mem i !prefixes then ( (*  printf "found!\n"; *) M.find i !prefixes )
    else
      let s = String.make (tabc*i) ' ' in
(*      printf "created prefix %d\n" i; *)
      prefixes := M.add i s !prefixes;
      s
  in

  let rec visit ~level el = match el with
    | PCData d when bad_pcdata d -> bug "PCData value contains ]]>"
    | PCData d -> out (prefix level); out d
    | Element (name, attr, lst) ->
      let prefix = prefix level in
      out (sprintf "%s<%s " prefix name);
      let attr_str = String.concat " " (List.map string_of_attr attr) in
      out attr_str;
      let () = match lst with 
	| [PCData d] -> out (sprintf ">%s</%s>\n" d name)
	| [] -> out "/>\n"
	| _  -> 
	  out ">\n";
	  List.iter (visit ~level:(level+1)) lst;
	  out (sprintf "%s</%s>\n" prefix name) 
      in
      ()
  in
  visit ~level:0 root
