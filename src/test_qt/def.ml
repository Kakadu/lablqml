type cppobj
(*
external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"
 *)
(*
external stub_hasDataChanged: cppobj -> bool -> unit = "caml_Controller_hasDataChanged_cppmeth_wrapper"
                                                       *)
class virtual controller = object(self)

  method virtual onItemSelected: int -> int -> unit[@@qtmeth]
  method virtual setPaths     : string list -> unit[@@qtmeth]
  method virtual linkActivated     : string -> unit[@@qtmeth]
  method virtual paths     : unit   -> string list[@@qtmeth]

  method virtual backTo    : string -> int -> unit[@@qtmeth]
  method virtual forwardTo : string -> int -> unit[@@qtmeth]
  method virtual getDefaultLibraryPath: unit -> string[@@qtmeth]

  method virtual hasData : bool[@@qtprop]
  method virtual descr   : string[@@qtprop]
  method virtual fullPath: string[@@qtprop]


end[@@qtclass][@@itemmodel]

