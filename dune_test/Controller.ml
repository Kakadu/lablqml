open Lablqml

class virtual controller =
  object (self)
    method virtual descr : string [@@qtprop]
  end [@@qtclass]

module IntModel = struct
  class virtual c = object (self) end [@@qtclass] [@@itemmodel]
end

module DataItem = struct
  class virtual dataItem =
    object (self)
      method virtual text : string [@@qtprop]

      method virtual cellX : int [@@qtprop]
    end [@@qtclass]
end
