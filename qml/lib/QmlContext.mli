type t
val get_view_exn : name:string -> t
val get_view : name:string -> t option

type cppobj
val set_context_property: ctx:t -> name:string -> cppobj -> unit

module QVariant : sig
  type t
  val empty: t
  val of_string: string -> t
  val of_object: cppobj -> t
end
module QModelIndex : sig
    type t
    val empty: t
    val row: t -> int
    val column: t -> int
    val make: row:int -> column:int -> t
end
