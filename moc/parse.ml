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

module Yaml2 = struct
  open Core
  open Sexplib.Conv

  module Types = struct
    type typ = TypAst.t with sexp
    type meth = string * typ list * typ * [`Const] list with sexp
    type prop = {name:string; getter:string; setter: string option; notifier: string; typ:typ} with sexp
    type clas =
        {classname:string; slots: meth list; signals: meth list; members: meth list;
         basename: string option; props: prop list}
    with sexp
    type data = clas list with sexp
  end

  open YamlNode

  let rec parse_data = function
    | MAPPING(_,lst) -> List.map lst ~f:parse_clas
    | _ -> assert false
  and parse_clas = function
    | (SCALAR (_, classname), MAPPING(_,lst)) ->
        let basename = ref None in
        let (members,signals,slots,props) =
          List.fold_left ~init:([],[],[],[]) lst ~f:(fun (a,b,c,d) -> function
            | (SCALAR(_,"basename"),SCALAR(_,value)) -> basename:= Some value; (a,b,c,d)
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
        Types.({classname;members;signals;slots;props; basename= !basename})
    | _ -> assert false
  and parse_meth = function
    | (SCALAR(_,name),SEQUENCE(_,lst)) ->
        let lst = List.map lst ~f:(function SCALAR (_,x) -> x | _ -> assert false) in
        (* last item in method return type *)
        let res,args =
          let l = List.rev lst in (List.hd_exn l, l |> List.tl_exn |> List.rev)
        in
        let conv ty = TypLexer.parse_string ty in
        (* TODO: parse `Const modifier *)
        (name,List.map args ~f:conv, conv res,[])
    | _ -> assert false
  and parse_prop = function
    | (SCALAR(_,name),MAPPING(_,lst)) ->
        let helper_exn name =
          lst |> List.filter_map ~f:(function
            | (SCALAR(_,x),SCALAR(_,value)) when x=name -> Some value
            | _ -> None)
          |> (function
             | [] -> raise Not_found
             | x::_ -> x)
        in
        let helper name = try Some (helper_exn name) with Not_found -> None in
        let getter   = helper_exn "get"
        and setter   = helper     "set"
        and typ      = helper_exn "type"
        and notifier = helper_exn "notify" in
        let typ = TypLexer.parse_string typ in
        Types.({name;getter;setter;notifier;typ})
    | _ -> assert false

  let parse_file filename : Types.clas list =
    let p = YamlParser.make () in
    let data = YamlParser.parse_string p Core.In_channel.(input_all (create filename)) in
    parse_data data

end
