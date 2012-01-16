(* Parsing config file *)
open Core
open Std_internal
module List = Core_list
module String = Core_string
open Printf
type meth = string * string list with sexp

let (|>) a f = f a 

let is_good_arg name = match name with
  | "unit" | "float" | "int" | "string" | "bool" -> true
  | _ when name.[0] = 'q' -> true
  | _ -> false

let parse_slot x =
  let prefix = "let " in
    printf "Parsing line: %s\n" x;
    match String.split ~on:':' x with
      | [name;typ] -> let args = Str.split_delim (Str.regexp "->") typ |> List.map ~f:String.strip in
		      let name = if String.is_prefix name ~prefix then String.drop_prefix name 4
			else name in
                      let name = String.strip name in
		      if List.for_all args ~f:is_good_arg then
			Some (name,args)
		      else ( (* printf "not all args are corrent\n"; *) None)
      | _ -> print_endline "Malformed line"; None

let parse_slots = List.filter_map ~f:parse_slot
let parse filename = read_lines filename |> parse_slots

type api_content =
    (string * meth list) list
with sexp

let parse2 filename =
  let lines = read_lines filename |> List.map ~f:String.strip in
  let ans: api_content ref = ref [] in
  let add_class c = match c with
    | Some (name, lst) -> ans := (name, List.rev lst) :: !ans
    | None -> assert false
  in
  let is_clasdecl = String.is_prefix ~prefix:"class " in
  let is_methdecl = String.is_prefix ~prefix:"let " in
  let last = List.fold_left lines ~init:None ~f:(fun acc x -> match (acc, is_clasdecl x, is_methdecl x) with
    | _,false,false -> Printf.printf "Malformed line: `%s`\n" x; acc
    | _,true,true   -> assert false
    | None,true,_ -> Some (String.drop_prefix x 6,[])
    | Some _,true,_ -> add_class acc;
      let new_class = String.drop_prefix x 6 in
      Some (new_class,[])
    | None,_,true   -> Printf.printf "meth declaration without a class: `%s`\n" x; acc
    | Some(name,lst),_,true -> begin
      match parse_slot x with
        | Some s -> Some (name, s::lst)
        | None -> acc
    end
  ) in
  let () = add_class last in
  List.rev !ans
