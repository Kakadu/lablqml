(* Generated at 0-9 13:38:17 *)

open QmlContext

external stub_cellXChanged: cppobj -> int -> unit = "caml_DataItem_cellXChanged_cppmeth_wrapper"
external stub_textChanged: cppobj -> string -> unit = "caml_DataItem_textChanged_cppmeth_wrapper"
external store: cppobj -> < .. > -> unit = "caml_store_value_in_DataItem"

class virtual base_DataItem cppobj = object(self)
  initializer store cppobj self
  method handler = cppobj
  (* Q_PROPERTY(int cellX  READ cellX NOTIFY cellXChanged) *)
  method emit_cellXChanged = stub_cellXChanged self#handler
  method virtual cellX: unit -> int
  (* don't decare properties without setter *)
  (* Q_PROPERTY(QString text  READ text NOTIFY textChanged) *)
  method emit_textChanged = stub_textChanged self#handler
  method virtual text: unit -> string
  (* don't decare properties without setter *)
end

external create_DataItem: unit -> 'a = "caml_create_DataItem"

