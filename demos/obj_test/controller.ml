open QmlContext

class virtual controller = object(self)
  method virtual getobj: unit -> QVariant.t[@@qtmeth]
  method virtual setobj: QVariant.t -> unit[@@qtmeth]

  method virtual person: QVariant.t[@@qtprop]

end[@@qtclass]
