(* Parsing config file *)
open Core
open Std_internal
module List = Core_list
module String = Core_string
open Printf
type meth = string * string list

let (|>) a f = f a 

let is_good_arg name = match name with
  | "unit" | "float" | "int" | "string" | "bool" -> true
  | _ when name.[0] = 'q' -> true
  | _ -> false

let parse_slots = 
  let prefix = "let " in
  List.filter_map ~f:(fun x -> 
    printf "Parsing line: %s\n" x;
    match String.split ~on:':' x with
      | [name;typ] -> let args = Str.split_delim (Str.regexp "->") typ |> List.map ~f:String.strip in
		      let name = if String.is_prefix name ~prefix then String.drop_prefix name 4
			else name in
		      if List.for_all args ~f:is_good_arg then
			Some (name,args)
		      else ( (* printf "not all args are corrent\n"; *) None)
      | _ -> print_endline "Malformed line"; None
)

let parse filename = 
  read_lines filename |> parse_slots
  
