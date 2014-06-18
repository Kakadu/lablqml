open QmlContext

class virtual controller = object(self)

  method virtual onItemSelected: int -> int -> unit[@@qtmeth]
  method virtual setPaths     : string list -> unit[@@qtmeth]
  method virtual paths     : unit   -> string list[@@qtmeth]

  method virtual hasData : bool[@@qtprop]
  method virtual descr   : string[@@qtprop]
  method virtual fullPath: string[@@qtprop]

end[@@qtclass]
