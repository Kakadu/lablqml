open QmlContext

class virtual controller = object(self)
  method virtual hiGotten: message:string -> unit[@@qtsignal]
  method virtual onMouseClicked: unit -> unit[@@qtmeth]
  method virtual clicksCount: int[@@qtprop]

end[@@qtclass]

