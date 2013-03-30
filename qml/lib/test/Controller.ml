(* Generated at 2013-03-30 14:13:59.890431 *)

open QmlContext

external stub_hasDataChanged: cppobj -> bool -> unit = "caml_Controller_hasDataChanged_cppmeth_wrapper"
external stub_descChanged: cppobj -> string -> unit = "caml_Controller_descChanged_cppmeth_wrapper"

class virtual base_Controller cppobj = object(self)
  initializer set_caml_object cppobj self
  method handler = cppobj
  method virtual onItemSelected: int->int-> unit
  method emit_hasDataChanged = stub_hasDataChanged self#handler
  method virtual isHasData: unit -> bool
  method emit_descChanged = stub_descChanged self#handler
  method virtual getDescr: unit -> string
end

external create_Controller: unit -> 'a = "caml_create_Controller"

