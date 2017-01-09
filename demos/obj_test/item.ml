open Lablqml

class virtual item = object(self)
  method virtual name: string[@@qtprop]
  method virtual age : int[@@qtprop]

end[@@qtclass]
