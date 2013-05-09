(* Generated at 2013-05-09 12:56:37.800855+04:00 *)

open QmlContext

external stub_hasDataChanged: cppobj -> bool -> unit = "caml_Controller_hasDataChanged_cppmeth_wrapper"
external stub_descChanged: cppobj -> string -> unit = "caml_Controller_descChanged_cppmeth_wrapper"
external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"

class virtual base_Controller cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method virtual onItemSelected: int->int-> unit
  method virtual setPaths: string list-> unit
  method virtual paths: unit-> string list
  method virtual getFullPath: unit-> string
  method emit_hasDataChanged = stub_hasDataChanged self#handler
  method virtual isHasData: unit -> bool
  method emit_descChanged = stub_descChanged self#handler
  method virtual getDescr: unit -> string
end

external create_Controller: unit -> 'a = "caml_create_Controller"

