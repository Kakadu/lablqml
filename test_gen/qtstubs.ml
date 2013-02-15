open Stub_helpers

external get_caml_object: [`qobject] obj -> 'a option = "hasCamlObj" 
external set_caml_object: [`qobject] obj -> < .. > -> unit = "setCamlObj"
external get_class_name : [`qobject] obj -> string option  = "getClassName"

module QApplication = struct
  type t = [`qapplication]
  external create : string array -> t obj = "ml_qapp_create"
  external exec : [> `qapplication] obj -> int = "ml_qapp_exec"
end



