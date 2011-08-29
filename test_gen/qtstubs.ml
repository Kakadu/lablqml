open Stub_helpers

external make_root_widget : unit -> [`qobject] obj = "make_root_widget"

module QApplication = struct
  type t = [`qapplication]
  external create : string array -> t obj = "ml_qapp_create"
  external exec : [> `qapplication] obj -> int = "ml_qapp_exec"
end
