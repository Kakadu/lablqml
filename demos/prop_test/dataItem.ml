open Lablqml

class virtual dataItem = object(self)
  method virtual text: string[@@qtprop]
  method virtual cellX: int[@@qtprop]

end[@@qtclass]
