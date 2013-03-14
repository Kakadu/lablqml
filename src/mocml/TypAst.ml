open Sexplib.Conv
open Core.Std
open Printf

let (|>) = Core.Fn.(|!)

type t = [`QModelIndex | `Bool | `Unit | `String | `Int | `Float | `Tuple of t list | `List of t  ] with sexp

let aux_variables_count (x : t) =
  let rec h = function
    | `QModelIndex
    | `Unit -> 0
    | `String -> 0
    | `Int -> 0
    | `Float  -> 0
    | `Bool -> 0
    | `Tuple lst -> List.fold_left ~f:(fun acc x -> max (h x) acc) ~init:(List.length lst) lst
    | `List t -> 2 + (h t)
  in
  h x

let is_primitive = function
  | `Float
  | `String | `Int | `Bool | `Unit  -> true
  | `QModelIndex
  | `Tuple _ | `List _ -> false

let rec to_cpp_type (typ:t) = match typ with
  | `Float  -> "double"
  | `String -> "QString"
  | `QModelIndex -> "QModelIndex"
  | `Int    -> "int"
  | `Bool   -> "bool"
  | `Unit   -> "void"
  | `Tuple lst when List.length lst <> 2 -> assert false
  | `Tuple xs  ->
      let a = xs |> List.hd_exn and b = xs |> List.tl_exn |> List.hd_exn in
      sprintf "QPair<%s,%s%s>"
        (to_cpp_type a) (to_cpp_type b)
        (if is_primitive b then "" else " ")
  | `List t -> sprintf "QList<%s%s>" (to_cpp_type t) (if is_primitive t then "" else " ")
  (*
let is_simple = function
  | `Int | `Float | `Bool | `String -> true
  | _ -> false
      *)
let to_ocaml_type typ =
  let rec helper = function
    | `Float  -> "float"
    | `Int    -> "int"
    | `String -> "string"
    | `Bool   -> "bool"
    | `Unit   -> "unit"
    | `QModelIndex -> "QModelIndex.t"
    | `Tuple xs ->
        List.map xs ~f:(fun x -> if is_primitive x then helper x else sprintf "(%s)" (helper x) )
          |> String.concat ~sep:"*"
    | `List (`Tuple xs) ->
        sprintf "(%s) list" (helper (`Tuple xs))
    | `List t -> sprintf "%s list" (helper t)
  in
  helper typ
(*  Buffer.contents b *)
