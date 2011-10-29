type +'a obj


external create_qwidget' : [`qobject] obj option -> [`qobject ] obj = "native_pub_createeee_QWidget_twin_QWidget_Qt_WindowFlags"
external qWidget_keyPressEvent': 'a->[> `qobject] obj->unit
                = "native_prot_QWidget_keyPressEvent_QKeyEvent"
    
class qKeyEvent me = object 
  method handler : [`qobject] obj = me
end
class qwidget me = object (self)
  method handler : [`qobject] obj = me
  method keyPressEvent: qKeyEvent  -> unit = fun x0 -> qWidget_keyPressEvent' self#handler (x0#handler)
end
(*
class b = object (self) 
  inherit a as super
  method boo () = print_endline "b.boo"

end
  *)
