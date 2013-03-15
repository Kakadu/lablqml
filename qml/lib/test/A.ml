(* Generated at 2013-03-16 00:32:51.303915 *)

open QmlContext

class type typ_for_A = object
  method sizey: unit-> int
  method parent: QModelIndex.t-> QModelIndex.t
  method index: int->int->QModelIndex.t-> QModelIndex.t
  method columnCount: QModelIndex.t-> int
  method rowCount: QModelIndex.t-> int
  method hasChildren: QModelIndex.t-> bool
  method data: QModelIndex.t->int-> QVariant.t
end
external create_A: typ_for_A -> 'a = "caml_create_A"
external add_role: 'a -> int -> string -> unit = "caml_A_addRole"
