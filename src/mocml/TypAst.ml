open Sexplib.Conv
open Core.Std
open Printf

let (|>) = Core.Fn.(|!)

type t =
  [`QModelIndex | `Bool | `Unit | `String | `Int | `Float | `Tuple of t list | `List of t
  | `QVariant
  ] with sexp


let aux_variables_count (x : t) =
  let rec h = function
    | `QModelIndex
    | `QVariant -> 0
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
  | `QVariant
  | `Tuple _ | `List _ -> false

let rec to_cpp_type (typ:t) = match typ with
  | `Float  -> "double"
  | `String -> "QString"
  | `QModelIndex -> "QModelIndex"
  | `QVariant -> "QVariant"
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
    | `QVariant    -> "QVariant.t"
    | `Tuple xs ->
        List.map xs ~f:(fun x -> if is_primitive x then helper x else sprintf "(%s)" (helper x) )
          |> String.concat ~sep:"*"
    | `List (`Tuple xs) ->
        sprintf "(%s) list" (helper (`Tuple xs))
    | `List t -> sprintf "%s list" (helper t)
  in
  helper typ
(*  Buffer.contents b *)

let to_verbose_typ =
  let open Parser in
  let rec helper = function
    | `Float  -> {t_name="float"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `Int    -> {t_name="int"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `String -> {t_name="QString"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `Bool   -> {t_name="bool"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `Unit   -> {t_name="void"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `QVariant->{t_name="QVariant"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `QModelIndex ->
        {t_name="QModelIndex"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }
    | `Tuple [a;b] ->
        {t_name="QPair"; t_is_const=false; t_indirections=0; t_is_ref=false;
         t_params= [ helper a; helper b ]
        }
    | `Tuple _ -> raise (Bug "can't convert tuple to C++ type")
    | `List x ->
        {t_name="QList"; t_is_const=false; t_indirections=0; t_is_ref=false;
         t_params=[ helper x ]
        }
  in
  helper

exception Cant_convert_cpptype of Parser.cpptype
let of_verbose_typ_exn =
  let open Parser in
  let rec helper = function
    | {t_name="float";  t_indirections=0;_} -> `Float
    | {t_name="int";    t_indirections=0;_} -> `Int
    | {t_name="QString";t_indirections=0;_} -> `String
    | {t_name="bool";   t_indirections=0;_} -> `Bool
    | {t_name="void";   t_indirections=0;_} -> `Unit
    | {t_name="QModelIndex"; t_indirections=0; _} -> `QModelIndex
    | {t_name="QVariant"; t_indirections=0; _} -> `QVariant
    | {t_name="QPair"; t_indirections=0; t_params=[l;r]; _} ->
        `Tuple (List.map [l;r] ~f:helper)
    | {t_name="QList"; t_indirections=0; t_params=[p]; _} ->
        `List (helper p)
    | x -> raise (Cant_convert_cpptype x)
  in
  helper

let of_verbose_typ x =
  try Some(of_verbose_typ_exn x)
  with Cant_convert_cpptype _ -> None
