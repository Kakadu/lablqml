open Ppxlib
open Format

val ref_append : set:'a list ref -> 'a -> unit

type opt_item =
  | OInstantiable
  | OItemModel
  | OItemModelVal of string option

module Options : sig
  type item = opt_item
  type t = item list

  val is_itemmodel : t -> bool
end

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

  val default_plus_model : default t -> [ default | model ] t
end

val ocaml_ast_of_typ : Arg.any Arg.t -> Longident.t

type meth_info =
  { mi_virt : bool
  ; mi_const : bool
  }

type arg_info =
  { ai_ref : bool
  ; ai_const : bool
  }

val ai_empty : arg_info

val gen_meth
  :  ?minfo:meth_info
  -> ?options:Options.t
  -> classname:string
  -> methname:string
  -> Arg.non_cppobj Arg.t list
  -> unit

val gen_signal
  :  classname:string
  -> signalname:string
  -> (string * Arg.non_cppobj Arg.t) list
  -> unit

val alloc_and_store
  :  Format.formatter
  -> classname:string
  -> obj:string
  -> where:string
  -> unit

val gen_prop : classname:string -> propname:string -> Arg.default Arg.t -> unit
val only_open : classname:string -> unit
val print_header_preamble : classname:string -> unit
val print_source_preamble : classname:string -> unit
val get_header_ch : classname:string -> out_channel
val get_source_ch : classname:string -> out_channel
val get_header_ppf : classname:string -> Format.formatter
val get_source_ppf : classname:string -> Format.formatter

val get_smart_ppf
  :  (classname:'a -> out_channel)
  -> classname:'a
  -> Format.formatter * (string -> unit)

val open_files : options:Options.t -> classname:string -> unit
val close_files : ?caml_owner:bool -> options:Options.t -> unit -> unit

module Names : sig
  val signal_of_prop : string -> string
  val getter_of_prop : string -> string
  val setter_of_prop : string -> string
end

val itemmodel_members : (string * Arg.non_cppobj Arg.t list * meth_info) list
val itemmodel_externals : classname:string -> (string * string * Arg.any Arg.t list) list
val gen_itemmodel_stuff : classname:string -> unit
val cpptyp_of_proptyp : Arg.default Arg.t * arg_info -> string
val wrap_typ_simple : 'a -> 'a * arg_info

type triplet

val cpp_value_of_ocaml
  :  ?options:opt_item list
  -> cppvar:string
  -> ocamlvar:string
  -> formatter
  -> triplet
  -> [ `Default | `Model ] Arg.t
  -> unit

val vars_triplet : string list -> triplet
val cpptyp_of_typ : [ `Default | `Model ] Arg.t * arg_info -> tag

val ocaml_value_of_cpp
  :  formatter
  -> triplet
  -> ocamlvar:tag
  -> cppvar:tag
  -> [ `Default | `Model ] Arg.t
  -> unit

val gen_stub_cpp
  :  ?options:opt_item list
  -> classname:tag
  -> stubname:tag
  -> methname:tag
  -> formatter
  -> (Arg.non_cppobj Arg.t * arg_info) list
  -> unit

val enter_blocking_section : formatter -> unit
val leave_blocking_section : formatter -> unit
