(* Parsing config file *)
open Core_kernel
open Core_kernel.Std

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
  open Core_kernel
  open Sexplib.Conv
  open ParserTypes

  module Types = struct
    type typ = cpptype [@@deriving sexp]
    type meth = string * typ list * typ * [`Const] list [@@deriving sexp]
    type sgnl = string * typ list * string list option [@@deriving sexp]
    type prop = {name:string; getter:string; setter: string option; notifier: string; typ:typ} [@@deriving sexp]
    type clas =
        {classname:string; slots: meth list; signals: sgnl list; members: meth list;
         basename: string option; props: prop list} [@@deriving sexp]

    type data = clas list [@@deriving sexp]
  end
end

module Json = struct
  open Yaml2
  open Yojson.Basic

  exception ParseError of string * json
  let parse_error msg json = raise (ParseError(msg,json))

  let js_get_string = function `String s -> s | _ -> assert false

  let assoc_and_map where name ~f err =
    match List.Assoc.find where name with
    | Some x -> f x
    | None -> err ()

  let rec parse_root j = match j with
    | `List [ xs ] -> List.map xs ~f:parse_class
    | _ -> assert false

  and parse_class (j: json) =
    match j with
      | `Assoc xs ->
          let basename = List.Assoc.find xs "basename"
             |> Option.map ~f:(function `String s -> s | _ -> assert false) in
          let classname =
            try List.Assoc.find_exn xs "classname" |> (function `String s -> s | _ -> assert false)
            with Not_found -> parse_error "Field 'classname' is required but not provided" j
          in
          let members = assoc_and_map xs "methods"
            ~f:(function
                 | `List ys  -> ys |> List.map ~f:(function `Assoc x -> x | _ -> assert false)
                                   |> List.map ~f:parse_meth
                 | x ->
                     pretty_to_channel stdout x;
                     assert false)
            (fun () -> printf "In '%s' field 'methods' is not declared and will be empty\n" classname; [])in
          let slots = assoc_and_map xs "slots"
            ~f:(function
                 | `List ys  -> ys |> List.map ~f:(function `Assoc x -> x | _ -> assert false)
                                   |> List.map ~f:parse_meth
                 | x -> raise (ParseError("List expected", x))

                     )
            (fun () -> printf "In '%s' field 'slots' is not declared and will be empty\n" classname; []) in
          let props = assoc_and_map xs "properties"
            ~f:(function
                 | `List ys  -> ys |> List.map ~f:(function `Assoc x -> x | _ -> assert false)
                                   |> List.map ~f:parse_prop
                 | x ->
                     pretty_to_channel stdout x;
                     assert false)
            (fun () -> printf "In '%s' field 'properties' is not declared and will be empty\n" classname;
              []) in
          let signals = assoc_and_map xs "signals"
            ~f:(function
                 | `List ys  -> ys |> List.map ~f:(function `Assoc x -> x | _ -> assert false)
                                   |> List.map ~f:parse_sgnl
                 | x ->
                     pretty_to_channel stdout x;
                     assert false)
            (fun () -> printf "In '%s' field 'signals' is not declared and will be empty\n" classname; [])in
          Types.({classname; members; signals; slots; props; basename})
      | _ -> assert false

  and parse_sgnl js =
    let name =
      try List.Assoc.find_exn js "name" |> js_get_string
      with Not_found -> parse_error "No field 'name' in signal description" (`Assoc js)
    in
    let f name = List.Assoc.find_exn js name |> (function `List xs -> xs | _ -> assert false)
      |> List.map ~f:(function `String s -> s | _ -> assert false)
    in
    let args = f "args" |> List.map ~f:(fun typ ->
      try typ |> TypLexer.parse_string_exn |> TypAst.to_verbose_typ
      with Failure msg -> parse_error (sprintf "failure during parsing '%s':\n%s" typ msg) (`Assoc js)
    ) in
    match List.Assoc.find js "argnames" with
    | Some xs ->
      let argnames = xs |> (function `List xs -> xs | _ -> assert false)
        |> List.map ~f:(function `String s -> s | _ -> assert false) in
      if List.(length argnames <> length args)
      then raise (Common.Bug (sprintf "In signal '%s': count of argument types should be  equal to count of argument names or arguments names should not be provded" name));
      (name,args,Some argnames)
    | None -> (name,args,None)

  and parse_meth js =
    let name =
      try List.Assoc.find_exn js "name" |> js_get_string
      with Not_found -> parse_error "No field 'name' in method description" (`Assoc js)
    in
    let sign =
      try List.Assoc.find_exn js "signature"
      with Not_found -> parse_error "No field 'signature' in method description" (`Assoc js)
    in
    let ys =
      try sign |> (function `List xs -> xs | _ -> assert false)
               |> List.map ~f:(function `String s -> s | _ -> assert false)
      with _ -> parse_error "Signature of method should list of strings" sign
    in

    (* last item in method return type *)
    let res,args =
      let l = List.rev ys in (List.hd_exn l, l |> List.tl_exn |> List.rev)
    in
    let conv ty =
      try ty |> TypLexer.parse_string_exn |> TypAst.to_verbose_typ
      with Failure msg -> parse_error (sprintf "failure during parsing '%s':\n%s" ty msg) (`Assoc js)
    in
    (* TODO: parse `Const modifier *)
    (name, List.map args ~f:conv, conv res,[])

  and parse_prop xs =
    let helper_exn name =
      try List.Assoc.find_exn xs name |> (function `String s  -> s|_ -> assert false)
      with Not_found -> parse_error (sprintf "No field '%s' in property description" name) (`Assoc xs)
    in
    let helper name =
      try Some (List.Assoc.find_exn xs name |> (function `String s  -> s| _ -> assert false))
      with Not_found -> None
    in
    let name     = helper_exn "name" in
    let getter   = helper_exn "get" in
    let setter   = helper     "set" in
    let typ      = helper_exn "type" in
    let notifier = helper_exn "notify" in
    let typ =
      try typ |> TypLexer.parse_string_exn |> TypAst.to_verbose_typ
      with Failure msg -> parse_error (sprintf "failure during parsing '%s':\n%s" typ msg) (`Assoc xs)
    in
    Types.({name;getter;setter;notifier;typ})

  let parse_file filename =
    Yojson.Basic.from_file filename |> (function `List xs -> List.map xs ~f:parse_class | _ -> assert false)
end
