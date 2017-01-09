open Lablqml

class virtual dataItem = object
  method virtual name: string [@@qtprop]
  method virtual sort: string [@@qtprop]
end[@@qtclass]
