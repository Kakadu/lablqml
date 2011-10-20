open Stub_helpers

external get_caml_object: [`qobject] obj -> 'a option
external set_caml_objet: [`qobject] obj -> <..> -> unit

module QApplication = struct
  type t = [`qapplication]
  external create : string array -> t obj = "ml_qapp_create"
  external exec : [> `qapplication] obj -> int = "ml_qapp_exec"
end
