type arg_info =
  { ai_ref : bool
  ; ai_const : bool
  }

type meth_info =
  { mi_virt : bool
  ; mi_const : bool
  }

val mi_empty : meth_info
val ai_empty : arg_info
val wrap_typ_simple : 'a -> 'a * arg_info
val unref : 'a * arg_info -> 'a * arg_info
val unconst : 'a * arg_info -> 'a * arg_info

module Arg : sig
  type default = [ `Default ]
  type model = [ `Model ]
  type cppobj = [ `Cppobj ]

  type non_cppobj =
    [ `Default
    | `Model
    ]

  type any =
    [ `Cppobj
    | `Default
    | `Model
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

  val default_plus_model : default t -> [ `Default | `Model ] t
  val model_plus_default : model t -> [ `Default | `Model ] t
  val remove_cppobj : any t -> non_cppobj t option
end

open Ppxlib

val string_suites_prop : string -> ([> `Default ] Arg.t, string) result
val type_suits_prop : core_type -> ([> `Default ] Arg.t, string) result

val eval_meth_typ_gen
  :  core_type
  -> ((arg_label * [> `Default ] Arg.t) list, string * location) result

val parse_arrow_type_exn : core_type -> (arg_label * [> `Default ] Arg.t) list

(* ***** *)
val aux_variables_count : Arg.non_cppobj Arg.t -> int
val aux_variables_count_to_cpp : Arg.non_cppobj Arg.t -> int
val cpptyp_of_typ : Arg.non_cppobj Arg.t * arg_info -> string
val cpptyp_of_proptyp : Arg.default Arg.t * arg_info -> string
val ocaml_ast_of_typ : [ `Cppobj | `Default | `Model ] Arg.t -> Ppxlib.longident
