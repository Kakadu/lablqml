open Ppxlib

val ref_append: set: 'a list ref -> 'a -> unit

type opt_item = OInstantiable | OItemModel | OItemModelVal of string option

module Options : sig
  type item = opt_item
  type t = item list
  val is_itemmodel: t -> bool
end

module Arg : sig
  type default = [ `Default ]
  type model = [ `Model ]
  type cppobj = [ `Cppobj ]
  type non_cppobj = [ `Default | `Model ]
  type any = [ `Cppobj | `Default | `Model ]

  type _ t =
    | Unit          : [> `Default ] t
    | QString       : [> `Default ] t
    | Int           : [> `Default ] t
    | Bool          : [> `Default ] t
    | QVariant      : [> `Default ] t
    | QByteArray    : [> `Default ] t
    | QList         : 'a t -> 'a t
    | QModelIndex   : [> `Model ] t
    | Cppobj        : [> `Cppobj ] t

end

val ocaml_ast_of_typ: Arg.any Arg.t -> Longident.t

type meth_info = { mi_virt: bool; mi_const: bool }
type arg_info = { ai_ref: bool; ai_const: bool }

val gen_meth: ?minfo:meth_info -> ?options:Options.t ->
  classname:string -> methname:string -> Arg.non_cppobj Arg.t list -> unit

val gen_signal: classname:string -> signalname:string -> (string * Arg.non_cppobj Arg.t) list -> unit
val gen_prop:  classname:string -> propname:string -> Arg.default Arg.t -> unit

val open_files: ?destdir:string -> ?ext:string -> options:Options.t -> classname:string -> unit
val close_files: options:Options.t -> unit

module Names : sig
  val signal_of_prop : string -> string
  val getter_of_prop : string -> string
  val setter_of_prop : string -> string
end

val itemmodel_members:
  (string * Arg.non_cppobj Arg.t list * meth_info) list

val itemmodel_externals: classname:string ->
  (string * string * Arg.any Arg.t list)
  list

val gen_itemmodel_stuff: classname:string -> unit
