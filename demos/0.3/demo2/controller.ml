open QmlContext

class virtual controller = object(self)
  method virtual onMouseClicked: unit -> unit[@@qtmeth]
  method virtual clicksCount: int[@@qtprop]
end[@@qtclass]

