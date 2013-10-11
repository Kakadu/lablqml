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
    type sgnl = string * typ list * string list option with sexp
    type prop = {name:string; getter:string; setter: string option; notifier: string; typ:typ} with sexp
    type clas =
        {classname:string; slots: meth list; signals: sgnl list; members: meth list;
         basename: string option; props: prop list}
    with sexp
    type data = clas list with sexp
  end
end

module Json = struct
  open Yaml2
  open Yojson.Basic

  let js_get_string = function `String s -> s | _ -> assert false

  let rec parse_root j = match j with
    | `List [ xs ] -> List.map xs ~f:parse_class
    | _ -> assert false
  and parse_class (j: json) =
    match j with
      | `Assoc xs ->
          let basename = List.Assoc.find xs "basename"
             |> Option.map ~f:(function `String s -> s | _ -> assert false) in
          let classname = List.Assoc.find_exn xs "classname"
             |> (function `String s -> s | _ -> assert false) in
          let members = List.Assoc.find_exn xs "methods"
             |> (function
                 | `List ys  -> ys |> List.map ~f:(function `Assoc x -> x | _ -> assert false)
                                   |> List.map ~f:parse_meth
                 | x ->
                     pretty_to_channel stdout x;
                     assert false ) in
          let slots : _ list = List.Assoc.find_exn xs "slots"
             |> (function
                 | `List xs -> List.map xs ~f:(function `Assoc x -> x | _ -> assert false)
                               |> List.map ~f:parse_meth
                 | _ -> assert false ) in
          let props = List.Assoc.find_exn xs "properties"
             |> (function
                 | `List xs -> List.map xs ~f:(function `Assoc x -> x | _ -> assert false)
                               |> List.map ~f: parse_prop
                 | _ -> assert false ) in
          let signals = List.Assoc.find_exn xs "signals"
             |> (function
                 | `List xs -> List.map xs ~f:(function `Assoc x -> x | _ -> assert false)
                               |>  List.map ~f:parse_sgnl
                 | _ -> assert false ) in
          Types.({classname; members; signals; slots; props; basename})
      | _ -> assert false
  and parse_sgnl js =
    let name = List.Assoc.find_exn js "name" |> js_get_string in
    let f name = List.Assoc.find_exn js name |> (function `List xs -> xs | _ -> assert false)
      |> List.map ~f:(function `String s -> s | _ -> assert false)
    in
    let args = f "args" |> List.map ~f:(fun x -> x|> TypLexer.parse_string |> TypAst.to_verbose_typ) in
    match List.Assoc.find js "argnames" with
    | Some xs ->
      let argnames = xs |> (function `List xs -> xs | _ -> assert false)
        |> List.map ~f:(function `String s -> s | _ -> assert false) in
      if List.(length argnames <> length args)
      then raise (Common.Bug (sprintf "In signal '%s': count of argument types should be  equal to count of argument names or arguments names should not be provded" name));
      (name,args,Some argnames)
    | None -> (name,args,None)
  and parse_meth (js: (string*json) list) =
    let name = List.Assoc.find_exn js "name" |> js_get_string in
    let sign = List.Assoc.find_exn js "signature" |> (function `List xs -> xs | _ -> assert false) in
    let ys = List.map sign ~f:(function `String s -> s | _ -> assert false) in
    (* last item in method return type *)
    let res,args =
      let l = List.rev ys in (List.hd_exn l, l |> List.tl_exn |> List.rev)
    in
    let conv ty = TypLexer.parse_string ty |> TypAst.to_verbose_typ in
    (* TODO: parse `Const modifier *)
    (name, List.map args ~f:conv, conv res,[])
  and parse_prop xs =
    let helper_exn s =
      try List.Assoc.find_exn xs s |> (function `String s  -> s|_ -> assert false)
      with Not_found as exn -> (* printf "%s is not set\n%!" s;*) raise exn
    in
    let helper name = try Some (helper_exn name) with Not_found -> None in
    let name = helper_exn "name" in
    let getter   = helper_exn "get" in
    let setter   = helper     "set" in
    let typ      = helper_exn "type" in
    let notifier = helper_exn "notify" in
    let typ = TypLexer.parse_string typ |> TypAst.to_verbose_typ in
    Types.({name;getter;setter;notifier;typ})

  let parse_file filename =
    Yojson.Basic.from_file filename |> (function `List xs -> List.map xs ~f:parse_class | _ -> assert false)
end
