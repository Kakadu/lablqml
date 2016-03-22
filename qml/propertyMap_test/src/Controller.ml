open QmlContext

class virtual controller = object(self)
  method virtual foo1 : unit -> unit[@@qtmeth]
  method virtual foo2 : unit -> unit[@@qtmeth]
end[@@qtclass]
