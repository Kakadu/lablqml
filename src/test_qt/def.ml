module QModelIndex = struct type t = int end
module QVariant    = struct type t = int end
type cppobj

class virtual controller = object(self)

  method virtual onItemSelected: int -> int -> unit[@@qtmeth]
  method virtual setPaths     : string list -> unit[@@qtmeth]
  method virtual linkActivated     : string -> unit[@@qtmeth]
  method virtual paths     : unit   -> string list[@@qtmeth]

  method virtual backTo    : string -> int -> unit[@@qtmeth]
  method virtual forwardTo : string -> int -> unit[@@qtmeth]
  method virtual getDefaultLibraryPath: unit -> string[@@qtmeth]

  method virtual hasData : bool[@@qtprop]
  method virtual descr   : string[@@qtprop]
  method virtual fullPath: string[@@qtprop]

end[@@qtclass]
(*                           
class virtual historyModel = object

 method virtual data: QModelIndex.t->int-> QVariant.t
end[@@itemmodel][@@qtclass]
*)
