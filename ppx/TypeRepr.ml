open Format
open Ppxlib
open Base
open PpxQtCfg

(* TODO: add addtitional info about methods *)
(* We need this arginfo because void foo(QString) is not the same as void foo(const QString&) *)
type arg_info =
  { ai_ref : bool
  ; ai_const : bool
  }

type meth_info =
  { mi_virt : bool
  ; mi_const : bool
  }

let mi_empty = { mi_virt = false; mi_const = false }
let ai_empty = { ai_ref = false; ai_const = false }
let wrap_typ_simple x = x, ai_empty
let unref (x, y) = x, { y with ai_ref = false }
let unconst (x, y) = x, { y with ai_const = false }

module Arg : sig
  type default = [ `Default ]
  type model = [ `Model ]
  type cppobj = [ `Cppobj ]

  type non_cppobj =
    [ default
    | model
    ]

  type any =
    [ cppobj
    | non_cppobj
    ]

  type _ t =
    | Unit : [> `Default ] t
    | QString : [> `Default ] t
    | Int : [> `Default ] t
    | Bool : [> `Default ] t
    | QVariant : [> `Default ] t
    | QByteArray : [> `Default ] t
    | QList : 'a t -> 'a t
    | QModelIndex : [> `Model ] t
    | Cppobj : [> `Cppobj ] t

  val default_plus_model : default t -> [ default | model ] t
  val model_plus_default : model t -> [ default | model ] t
  val remove_cppobj : any t -> non_cppobj t option
end = struct
  type default = [ `Default ]
  type model = [ `Model ]
  type cppobj = [ `Cppobj ]

  type non_cppobj =
    [ default
    | model
    ]

  type any =
    [ cppobj
    | default
    | model
    ]

  type _ t =
    | Unit : [> `Default ] t
    | QString : [> `Default ] t
    | Int : [> `Default ] t
    | Bool : [> `Default ] t
    | QVariant : [> `Default ] t
    | QByteArray : [> `Default ] t
    | QList : 'a t -> 'a t
    | QModelIndex : [> `Model ] t
    | Cppobj : [> `Cppobj ] t

  let rec model_plus_default : model t -> [ default | model ] t = function
    | QModelIndex as y -> y
    | QList x -> QList (model_plus_default x)
  ;;

  let rec cppobj_to_any : cppobj t -> any t = function
    | Cppobj -> Cppobj
    | QList x -> QList (cppobj_to_any x)
  ;;

  let rec model_to_any : model t -> any t = function
    | QModelIndex -> QModelIndex
    | QList x -> QList (model_to_any x)
  ;;

  let rec default_plus_model : default t -> [ default | model ] t = function
    | (Unit as y)
    | (QString as y)
    | (Int as y)
    | (Bool as y)
    | (QVariant as y)
    | (QByteArray as y) -> y
    | QList x -> QList (default_plus_model x)
  ;;

  let rec remove_cppobj : any t -> non_cppobj t option = function
    | Cppobj -> None
    | Unit -> Some Unit
    | Int -> Some Int
    | Bool -> Some Bool
    | QByteArray -> Some QByteArray
    | QString -> Some QString
    | QModelIndex -> Some QModelIndex
    | QVariant -> Some QVariant
    | QList xs ->
      (match remove_cppobj xs with
      | None -> None
      | Some y -> Some (QList y))
  ;;
end [@warning "-32"]

open Arg

let type_suits_prop ty =
  let open Result in
  match ty with
  | [%type: int] -> return Arg.Int
  | [%type: bool] -> return Arg.Bool
  | [%type: string] -> return Arg.QString
  | [%type: Variant.t] | [%type: QVariant.t] -> return Arg.QVariant
  | [%type: unit] -> fail "property can't have type 'unit'"
  | { ptyp_desc = Ptyp_constr ({ txt = Lident x; _ }, []); _ } ->
    fail (sprintf "Don't know what to do with '%s'" x)
  | _ -> fail "Type is unknown"
;;

(* return list of pairs. 1st is argument label. 2nd is actual type *)
let eval_meth_typ_gen =
  let open Result in
  let rec parse_one t =
    match t.ptyp_desc with
    | Ptyp_constr ({ txt = Lident "list"; _ }, [ typarg ]) ->
      parse_one typarg >>= fun xs -> return @@ Arg.QList xs
    | Ptyp_constr ({ txt = Lident "string"; _ }, _) -> return @@ Arg.QString
    | Ptyp_constr ({ txt = Lident "unit"; _ }, _) -> return @@ Arg.Unit
    | Ptyp_constr ({ txt = Lident "int"; _ }, _) -> return @@ Arg.Int
    | Ptyp_constr ({ txt = Lident "variant"; _ }, _)
    | Ptyp_constr ({ txt = Ldot (Lident "Variant", "t"); _ }, _)
    | Ptyp_constr ({ txt = Ldot (Lident "QVariant", "t"); _ }, _) -> return Arg.QVariant
    | _ -> fail ("can't eval type", t.ptyp_loc)
  in
  let rec helper t =
    match t.ptyp_desc with
    | Ptyp_arrow (name, l, r) ->
      parse_one l >>= fun rez -> helper r >>= fun tl -> return ((name, rez) :: tl)
      (* [ name, parse_one l ] @ helper r *)
    | _ -> parse_one t >>= fun typ -> return [ Nolabel, typ ]
  in
  helper
;;

let parse_arrow_type_exn typ =
  match eval_meth_typ_gen typ with
  | Result.Ok xs ->
    assert (List.length xs > 1);
    xs
  | Result.Error (msg, loc) -> ppxqt_failed ~loc msg
;;

(* how many additional variables needed to convert C++ value to OCaml one *)
let aux_variables_count =
  let rec helper : non_cppobj Arg.t -> int = function
    | QByteArray | Bool | Int | Unit | QString | QModelIndex -> 0
    | QList x -> helper x + 2
    | QVariant -> 2
  in
  helper
;;

(* how many additional variables needed to convert OCaml value to C++ one *)
let aux_variables_count_to_cpp =
  let rec helper : non_cppobj Arg.t -> int = function
    | QVariant | QByteArray | Bool | Int | Unit | QString | QModelIndex -> 0
    | QList x -> helper x + 2
  in
  helper
;;

let rec ocaml_ast_of_typ : any Arg.t -> Longident.t =
 fun x ->
  let open Longident in
  match x with
  | Cppobj -> Lident "cppobj"
  | QVariant -> Ldot (Lident "QVariant", "t")
  | QModelIndex -> Ldot (Lident "QModelIndex", "t")
  | Bool -> Lident "bool"
  | Unit -> Lident "unit"
  | QByteArray | QString -> Lident "string"
  | Int -> Lident "int"
  | QList x -> Lapply (Lident "list", ocaml_ast_of_typ x)
;;

let cpptyp_of_typ =
  let rec helper : non_cppobj Arg.t * arg_info -> string =
   fun (x, ai) ->
    match x with
    | Bool -> "bool"
    | Int -> "int"
    | QVariant -> "QVariant"
    | QString -> "QString"
    | QByteArray -> "QByteArray"
    | Unit -> "void"
    | QModelIndex ->
      sprintf
        "%sQModelIndex%s"
        (if ai.ai_const then "const " else "")
        (if ai.ai_ref then "&" else "")
    | QList x ->
      sprintf
        "%sQList<%s>%s"
        (if ai.ai_const then "const " else "")
        (helper (x, ai_empty))
        (if ai.ai_ref then "&" else "")
  in
  helper
;;

let rec cpptyp_of_proptyp : default Arg.t * arg_info -> string =
 fun (typ, ai) ->
  let upcasted = default_plus_model typ, ai in
  let cppname =
    match typ with
    | Bool -> cpptyp_of_typ upcasted
    | Int -> cpptyp_of_typ upcasted
    | QVariant -> cpptyp_of_typ upcasted
    | QByteArray -> cpptyp_of_typ upcasted
    | QString -> cpptyp_of_typ upcasted
    | Unit -> failwith "should not happen"
    | QList x -> sprintf "QList<%s>" (cpptyp_of_proptyp (x, ai_empty))
  in
  sprintf
    "%s%s%s"
    (if ai.ai_const then "const " else "")
    cppname
    (if ai.ai_ref then "&" else "")
;;

let string_suites_prop s =
  match s with
  | "int" -> Result.return Arg.Int
  | "bool" -> Result.return Arg.Bool
  | "string" -> Result.return Arg.QString
  | "variant" -> Result.return Arg.QVariant
  | _ -> Result.fail (Printf.sprintf "Type '%s' is unknown" s)
;;
