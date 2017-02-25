open Lablqml

class virtual demoClass1 = object(self)
  method virtual count: int[@@qtprop]
  method virtual tick : unit -> unit [@@qtmeth]
end[@@qtclass { metatype } ]
