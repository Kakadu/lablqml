(* Parsing config file *)
open Core
open Core.Std

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
let parse filename : (string * (string list)) list = 
  let ch = In_channel.create ~binary:false filename in
  let ans = In_channel.input_lines ch |> parse_slots in
  let () = In_channel.close ch in
  ans
  (*
type api_content =
    (string * meth list) list
with sexp

let with_file filename f = 
  let ch = In_channel.create ~binary:false filename in
  let ans = In_channel.input_lines ch |> f in
  let () = In_channel.close ch in
  ans

let parse2 filename =
  let lines = with_file filename (List.map ~f:String.strip) in
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
  *)
(*
let parse_yaml filename =
  let p = YamlParser.make () in
  let v = YamlParser.parse_string p Core.In_channel.(input_all (create filename)) in
    let rec to_string y =
      match y with
        | YamlNode.SCALAR (_, str) -> str
        | _ -> assert false
    and to_list y =
      match y with
        | YamlNode.SEQUENCE (_, seq) -> List.map ~f: to_string seq
        | _ -> assert false
    and to_meth_list y =
      match y with
        | YamlNode.MAPPING (_, map) ->
          List.rev (List.fold_left ~f: (fun acc (k, v) -> (to_string k, to_list v) :: acc) ~init: ([] : meth list) map)
        | _ -> assert false
    and parse y =
      match y with
        | YamlNode.MAPPING (_, map) ->
          List.rev (List.fold_left ~f: (fun acc (k, v) -> (to_string k, to_meth_list v) :: acc) ~init: ([] : api_content) map)
        | _ -> assert false
    in
    parse v
    *)

module Yaml2 = struct
  open Core
  open Sexplib.Conv

  module Types = struct
    type typ = [ `Simple of string | `List of string ] with sexp
    type meth = string * typ list * typ with sexp
    type prop = {name:string; getter:string; setter: string; notifier: string; typ:typ} with sexp
    type clas =
        {classname:string; slots: meth list; signals: meth list; members: meth list; props: prop list}
    with sexp
    type data = clas list with sexp
  end

  open YamlNode
  
  let rec parse_data = function
    | MAPPING(_,lst) -> List.map lst ~f:parse_clas
    | _ -> assert false
  and parse_clas = function
    | (SCALAR (_, classname), MAPPING(_,lst)) ->
        let (members,signals,slots,props) =
          List.fold_left ~init:([],[],[],[]) lst ~f:(fun (a,b,c,d) -> function
            | (SCALAR (_,"signals"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (a,lst @ b,c,d)
            | (SCALAR (_,"slots"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (a,b,lst@c,d)
            | (SCALAR (_,"methods"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (lst@a,b,c,d)
            | (SCALAR (_,"properties"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_prop in
                (a,b,c,lst@d)
            | _ -> assert false
          ) in
        Types.({classname;members;signals;slots;props})
    | _ -> assert false
  and parse_meth = function
    | (SCALAR(_,name),SEQUENCE(_,lst)) ->
        let lst = List.map lst ~f:(function SCALAR (_,x) -> x | _ -> assert false) in
        (* last item in method return type *)
        let res,args = 
          let l = List.rev lst in (List.hd_exn l, l |> List.tl_exn |> List.rev)
        in
        let conv ty = match String.split ~on:' ' ty with
          | [s] -> `Simple s 
          | [s; "list"] -> `List s
          | _ -> assert false
        in
        (name,List.map args ~f:conv, conv res)
    | _ -> assert false
  and parse_prop = function
    | (SCALAR(_,name),MAPPING(_,lst)) ->
        let helper name =
          lst |> List.filter_map ~f:(function
            | (SCALAR(_,x),SCALAR(_,value)) when x=name -> Some value
            | _ -> None)
    |> (function
        | [] -> assert false
        | x::_ -> x)
        in
        let getter = helper "get"
        and setter = helper "set"
        and typ = helper "type"
        and notifier = helper "notify" in
        let typ = match String.split ~on:' ' typ with
          | [s;"list"] -> `List s 
          | s -> `Simple typ
        in
        Types.({name;getter;setter;notifier;typ})
    | _ -> assert false

  let parse_file filename : Types.clas list =
    let p = YamlParser.make () in
    let data = YamlParser.parse_string p Core.In_channel.(input_all (create filename)) in
    parse_data data
    
end
