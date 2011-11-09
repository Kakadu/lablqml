type +'a obj

external create_app : string array -> [`qobject] obj = "ml_qapp_create"
external set_caml_object: [`qobject] obj -> < .. > -> unit = "setCamlObj"
external exec : unit -> int = "ml_qapp_exec"
external create_qwidget' : [`qobject] obj option -> [`qobject ] obj = "create_QWidget_twin"
external qWidget_keyPressEvent': 'a->[> `qobject] obj->unit
                = "qWidget_twin_super_keyPressEvent"
external qWidget_show : [`qobject] obj -> unit = "qWidget_twin_show"
(*
external nullObject : unit -> [`qobject] obj = "getNullObject"
external simpleQKeyEvent : unit -> [`qobject] obj = "myQKeyEvent"
*)

class qKeyEvent me = object 
  method handler : [`qobject] obj =
    print_endline "QKEYEVENT";
    me
end
class qwidget me = object (self)
  initializer set_caml_object me self
  method handler : [`qobject] obj = me
  method keyPressEvent (x0:qKeyEvent) = print_endline "PRESS";
    let _ = self#handler in
   qWidget_keyPressEvent' self#handler (x0#handler) 
  method show = qWidget_show me
end
(*
class b = object (self) 
  inherit a as super
  method boo () = print_endline "b.boo"

end
  *)
