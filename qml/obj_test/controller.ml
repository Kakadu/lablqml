open QmlContext

class virtual controller = object(self)
  method virtual getobj: unit -> QVariant.t[@@qtmeth]
  method virtual setobj: QVariant.t -> unit[@@qtmeth]
(*
  method virtual person: cppobj[@@qtprop]
 *)
end[@@qtclass]
