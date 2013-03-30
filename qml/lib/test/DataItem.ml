(* Generated at 2013-03-31 00:17:18.874502 *)

open QmlContext

external store: cppobj -> < .. > -> unit = "caml_store_value_in_DataItem"

class virtual base_DataItem cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method virtual name: unit-> string
  method virtual sort: unit-> string
end

external create_DataItem: unit -> 'a = "caml_create_DataItem"

