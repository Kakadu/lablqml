type 'a slot
type 'a signal

class qWidget : [ `qwidget] Qt.obj ->  object
  method show : unit -> unit 
  method handler: [`qwidget] Qt.obj
  (*method setObjectName : string -> unit *)
end
val createWidget: unit -> qWidget

class qPushButton: [ `qwidget] Qt.obj -> object 
  inherit qWidget
  method handler : [ `qwidget] Qt.obj
  method setCheckable : bool -> unit
  method setChecked   : bool -> unit  
  method clicked : < name:string; arg1:bool > signal
(*  method show : unit -> unit *)
end

val createButton : string -> string -> qPushButton 

class qApplication: [ `qwidget] Qt.obj -> object  
  method handler : [ `qwidget] Qt.obj
  method exec : unit -> int
  method quit : < name:string > slot
end

val createApp: string array -> qApplication

val connect : qPushButton ->  <name:string; ..> signal -> 
                 qApplication -> <name:string; ..> slot   -> unit 
(*
class qvboxlayout: [`qlayout] Qt.obj -> object
  method handler: [`qlayout] Qt.obj
  method addWidget: <handler:[`qwidget] Qt.obj; ..> -> unit 
end
val createVBoxLayout: qWidget option -> qvboxlayout
*)
