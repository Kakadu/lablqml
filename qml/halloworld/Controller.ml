(* Generated at 2013-09-09 23:09:08.791308+04:00 *)

open QmlContext

external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"

class virtual base_Controller cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method virtual onMouseClicked: unit-> unit
end

external create_Controller: unit -> 'a = "caml_create_Controller"

