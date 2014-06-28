(* Generated at 2013-05-31 18:51:49.451305+04:00 *)

open QmlContext

external store: cppobj -> < .. > -> unit = "caml_store_value_in_Controller"

class virtual base_Controller cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  method virtual onMouseClicked: string-> unit
end

external create_Controller: unit -> 'a = "caml_create_Controller"

