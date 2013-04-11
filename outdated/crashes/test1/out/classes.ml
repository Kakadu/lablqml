
open Stubs
open Stub_helpers
class virtual qKeyEvent me = object(self)
 method handler : [`qobject] obj = me 

  (* method int key( ) *)
  method key  =  qKeyEvent_key' self#handler 

end
and  qObject me = object(self)
 method handler : [`qobject] obj = me
 initializer Qtstubs.set_caml_object me self
  (* method QObject* parent( ) *)
  method parent  =  qObject_parent' self#handler 
|> (function Some o -> Some ( match Qtstubs.get_caml_object o with
                            | Some x -> (x:>qObject)
                            | None -> new qObject o)
   | None -> None)

  (* method void setParent(QObject*   ) *)
  method setParent (x0: qObject ) =  qObject_setParent' self#handler (x0#handler)

  (* method bool signalsBlocked( ) *)
  method signalsBlocked  =  qObject_signalsBlocked' self#handler 

end
and  qWidget me = object(self)
 method handler : [`qobject] obj = me
 initializer Qtstubs.set_caml_object me self
  method slot_show = object (_ : (<  .. >, unit ) #ssslot)
    method name = "show()"
  (* method void show( ) *)
  method call  =  qWidget_show' self#handler 

  end
  (* method void keyPressEvent(QKeyEvent*   ) *)
  method keyPressEvent (x0: qKeyEvent ) =  match Qtstubs.get_class_name me with
    | Some "QWidget" ->  qWidget_keyPressEvent' self#handler (x0#handler)
    | Some _ | None -> failwith "Calling protected method of non-twin type"

  (* method QObject* parent( ) *)
  method parent  =  qObject_parent' self#handler 
|> (function Some o -> Some ( match Qtstubs.get_caml_object o with
                            | Some x -> (x:>qObject)
                            | None -> new qObject o)
   | None -> None)

  (* method void setParent(QObject*   ) *)
  method setParent (x0: qObject ) =  qObject_setParent' self#handler (x0#handler)

  (* method bool signalsBlocked( ) *)
  method signalsBlocked  =  qObject_signalsBlocked' self#handler 

end
and  aa = object end