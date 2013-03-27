(* Generated at 2013-03-27 13:45:00.158189 *)

open QmlContext


class virtual base_Controller cppobj = object(self)
  initializer set_caml_object cppobj self
  method handler = cppobj
  method virtual onItemSelected: int->int-> unit
end

external create_Controller: unit -> 'a = "caml_create_Controller"

