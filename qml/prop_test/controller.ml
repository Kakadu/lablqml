open QmlContext

class virtual controller = object(self)
  method virtual onMouseClicked: unit -> unit[@@qtmeth]
  method virtual x: int[@@qtprop]
  method virtual y: int[@@qtprop]
  method virtual state: string[@@qtprop]

end[@@qtclass]
