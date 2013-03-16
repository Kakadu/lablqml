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
  open Parser

  module Types = struct
    type typ = Parser.cpptype with sexp
    type meth = string * typ list * typ * [`Const] list with sexp
    type prop = {name:string; getter:string; setter: string option; notifier: string; typ:typ} with sexp
    type clas =
        {classname:string; slots: meth list; signals: meth list; members: meth list;
         basename: string option; props: prop list}
    with sexp
    type data = clas list with sexp
  end

  open YamlNode
  let string_of_yaml y =
    let b = Buffer.create 100 in
    let add_string = Buffer.add_string b in
    let rec helper = function
      | SCALAR(uri,s) ->
          List.iter ~f:add_string [ "SCALAR(\""; uri; "\",\""; s; "\")" ]
      | SEQUENCE(uri,xs) ->
          List.iter ~f:add_string [ "SEQUENCE(\""; uri; "\",[" ];
          let () = match xs with
            | []  -> ()
            | [x] -> (helper x)
            | x::xs ->
                (helper x);
                List.iter xs ~f:(fun y -> add_string ","; helper y)
          in
          add_string "])"
      | MAPPING (uri,xs) ->
          List.iter ~f:add_string [ "MAPPING(\""; uri; "\",[" ];
          let add_pair x =
            add_string "("; helper (fst x); add_string ","; helper (snd x); add_string ")"
          in
          let () = match xs with
            | []  -> ()
            | [x] -> add_pair x
            | x::xs ->
                add_pair x;
                List.iter xs ~f:(fun y -> add_string ","; add_pair y)
          in
          add_string "])"
    in
    helper y;
    Buffer.contents b

  let rec parse_data = function
    | MAPPING(_,lst) ->
        printf "Classes count: %d\n" (List.length lst);
        List.map lst ~f:parse_clas
    | _ -> assert false
  and parse_clas = function
    | (SCALAR (_, classname), MAPPING(_,lst)) ->
        let basename = ref None in
        let (members,signals,slots,props) =
          List.fold_left ~init:([],[],[],[]) lst ~f:(fun (a,b,c,d) -> function
            | (SCALAR(_,"basename"),SCALAR(_,value)) -> basename:= Some value; (a,b,c,d)
            | (SCALAR (_,"signals"), SCALAR("null","")) ->
                (a,b,c,d)
            | (SCALAR (_,"signals"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (a,lst @ b,c,d)
            | (SCALAR (_,"slots"), SCALAR("null","")) ->
                (a,b,c,d)
            | (SCALAR (_,"slots"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (a,b,lst@c,d)
            | (SCALAR (_,"methods"),SCALAR ("null","")) ->
                (* methods not defined *)
                (a,b,c,d)
            | (SCALAR (_,"methods"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_meth in
                (lst@a,b,c,d)
            | (SCALAR (_,"properties"),SCALAR("null","")) ->
                (a,b,c,d)
            | (SCALAR (_,"properties"),MAPPING (_,map)) ->
                let lst = List.map map ~f:parse_prop in
                (a,b,c,lst@d)
            | x ->
                printf "Don't know what to do with\n(%s,%s)\n%!"
                  (string_of_yaml (fst x)) (string_of_yaml (snd x));
                assert false
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
        let conv ty = TypLexer.parse_string ty |> TypAst.to_verbose_typ in
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
        let typ = TypLexer.parse_string typ |> TypAst.to_verbose_typ in
        Types.({name;getter;setter;notifier;typ})
    | _ -> assert false

  let parse_file filename : Types.clas list =
    let p = YamlParser.make () in
    let data = YamlParser.parse_string p Core.In_channel.(input_all (create filename)) in
    parse_data data

end
