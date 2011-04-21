open Qt
type 'a slot = 'a
type 'a signal = 'a

external setButtonChecked:   'a -> bool -> unit = "ml_setButtonChecked"
external setButtonCheckable: 'a -> bool -> unit = "ml_setButtonCheckable" 

external qWidget_show : [> `qwidget] obj -> unit = "ml_qWidget_show"
class qWidget me = object (self)
  method show () = qWidget_show me
end

class qPushButton me = object(self)
  inherit qWidget me as super
  method handler:  [ `qwidget] obj  = me 
  method setChecked:   bool -> unit = setButtonChecked   me
  method setCheckable: bool -> unit = setButtonCheckable me 
  method show () = super#show ()

  method clicked: < name:string; arg1:bool > signal = 
    object  method name = "clicked()"  method arg1 = true end
end

external createButton': string -> [> `qwidget] obj  = "ml_createButton"
let createButton name text = 
  let w = createButton' text in
  QObject.setObjectName name w;
  new qPushButton w

(* application *)
external execApp  : [> `qwidget] obj -> int = "ml_QApplication_exec"
class qApplication me = object (self) 
  method handler:  [ `qwidget] obj  = me 
  method exec () = execApp me
  method quit : < name:string > slot = 
    object method name = "quit()" end
end

external createApp': string array -> [> `qwidget] obj = "ml_QApplication"
let createApp arr = 
  let app : [> `qwidget] obj = createApp' arr in
  new qApplication app
(* application end *)



let connect : qPushButton ->  <name:string; ..> signal -> 
                 qApplication -> <name:string; ..> slot   -> unit = 
  fun sender signal target slot ->
(*   print_endline ("signal = " ^ (signal#name) );
   print_endline ("slot   = " ^ (slot#name) ); *)
    let b = QObject.connect sender#handler ~signal:signal#name
      ~dst:target#handler ~slot:slot#name in ()
