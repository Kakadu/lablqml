(* Generated at 2013-05-09 12:56:37.801696+04:00 *)

open QmlContext

external stub_report_dataChanged: cppobj -> QModelIndex.t->QModelIndex.t->unit =
  "caml_AbstractModel_dataChanged_cppmeth_wrapper"
external stub_beginInsertRows: cppobj -> QModelIndex.t->int->int->unit =
  "caml_AbstractModel_beginInsertRows_cppmeth_wrapper"
external stub_endInsertRows: cppobj -> unit->unit =
  "caml_AbstractModel_endInsertRows_cppmeth_wrapper"
external stub_beginRemoveRows: cppobj -> QModelIndex.t->int->int->unit =
  "caml_AbstractModel_beginRemoveRows_cppmeth_wrapper"
external stub_endRemoveRows: cppobj -> unit->unit =
  "caml_AbstractModel_endRemoveRows_cppmeth_wrapper"
external store: cppobj -> < .. > -> unit = "caml_store_value_in_AbstractModel"

class virtual base_AbstractModel cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
 method report_dataChanged = stub_report_dataChanged cppobj
 method beginInsertRows = stub_beginInsertRows cppobj
 method endInsertRows = stub_endInsertRows cppobj
 method beginRemoveRows = stub_beginRemoveRows cppobj
 method endRemoveRows = stub_endRemoveRows cppobj
  method virtual parent: QModelIndex.t-> QModelIndex.t
  method virtual index: int->int->QModelIndex.t-> QModelIndex.t
  method virtual columnCount: QModelIndex.t-> int
  method virtual rowCount: QModelIndex.t-> int
  method virtual hasChildren: QModelIndex.t-> bool
  method virtual data: QModelIndex.t->int-> QVariant.t
end

external add_role: 'a -> int -> string -> unit = "caml_AbstractModel_addRole_cppmeth_wrapper"
external create_AbstractModel: unit -> 'a = "caml_create_AbstractModel"

