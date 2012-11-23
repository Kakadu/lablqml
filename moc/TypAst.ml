open Sexplib.Conv
open Core.Std
open Printf

let (|>) = Core.Fn.(|!)

type t = [ `Bool | `Unit | `String | `Int | `Float | `Tuple of t list | `List of t  ] with sexp

let aux_variables_count (x : t) =
  let rec h = function
    | `Unit -> 0
    | `String -> 0
    | `Int -> 0
    | `Float  -> 0
    | `Bool -> 0
    | `Tuple lst -> List.fold_left ~f:(fun acc x -> max (h x) acc) ~init:(List.length lst) lst
    | `List t -> 2 + (h t)
  in
  h x

let rec to_cpp_type (typ:t) = match typ with
  | `Float  -> "double"
  | `String -> "QString"
  | `Int    -> "int"
  | `Bool   -> "bool"
  | `Unit   -> "void"
  | `Tuple lst when List.length lst <> 2 -> assert false
  | `Tuple xs  -> 
      sprintf "QPair<%s,%s >" 
        (xs |> List.hd_exn |> to_cpp_type) (xs |> List.tl_exn |> List.hd_exn |> to_cpp_type)
  | `List t -> sprintf "QList<%s >" (to_cpp_type t)
  
let is_simple = function
  | `Int | `Float | `Bool | `String -> true
  | _ -> false

let to_ocaml_type typ =
  let rec helper = function
    | `Float  -> "float"
    | `Int    -> "int"
    | `String -> "string"
    | `Bool   -> "bool"
    | `Unit   -> "unit"
    | `Tuple xs ->
        List.map xs ~f:(fun x -> if is_simple x then helper x else sprintf "(%s)" (helper x) )
          |> String.concat ~sep:"*"
    | `List (`Tuple xs) ->
        sprintf "(%s) list" (helper (`Tuple xs))
    | `List t -> sprintf "%s list" (helper t)
  in
  helper typ
(*  Buffer.contents b *)



















