(* Generated at 2013-05-31 19:13:44.040810+04:00 *)

open QmlContext

external stub_nameChanged: cppobj -> string -> unit = "caml_DataItem_nameChanged_cppmeth_wrapper"
external stub_titleChanged: cppobj -> string -> unit = "caml_DataItem_titleChanged_cppmeth_wrapper"
external store: cppobj -> < .. > -> unit = "caml_store_value_in_DataItem"

class virtual base_DataItem cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method emit_nameChanged = stub_nameChanged self#handler
  method virtual author: unit -> string
  method emit_titleChanged = stub_titleChanged self#handler
  method virtual title: unit -> string
end

external create_DataItem: unit -> 'a = "caml_create_DataItem"

