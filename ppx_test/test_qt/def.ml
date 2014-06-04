
class type controller = object
  val hasData : bool
  val name: string
  method linkActivated : string -> unit
  method backTo : string -> int -> unit
  method getDefaultLibraryPath: unit -> string
end[@@qtclass]


(*
  val name : string
  method redraw : unit -> unit
end]
 *)
