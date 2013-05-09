(* Generated at 2013-05-09 20:17:43.137604+04:00 *)

open QmlContext

external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"

class virtual base_Controller cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method virtual onMouseClicked: unit-> unit
end

external create_Controller: unit -> 'a = "caml_create_Controller"

