type t
val get_view_exn : name:string -> t
val get_view : name:string -> t option

val set_context_property: ctx:t -> name:string -> 'a -> unit

module QModelIndex : sig
    type t
    val empty: t
    val row: t -> int
    val column: t -> int
end

