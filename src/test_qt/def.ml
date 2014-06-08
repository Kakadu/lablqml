type cppobj
(*
external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"
 *)
class virtual controller = object(self)
  method virtual hasData : bool[@@qtprop]
  (*val name: string*)
  method virtual linkActivated : string -> unit[@@qtmeth]
        (*
  method backTo : string -> int -> unit
  method getDefaultLibraryPath: unit -> string *)

end[@@qtclass]

