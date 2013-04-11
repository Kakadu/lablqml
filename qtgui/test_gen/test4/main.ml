open Qtstubs
open Classes
open Stubs
open Stub_helpers
open UserSlots_stubs

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)

let w1 = Creators.create_QWidget_0 None `Widget
let user_obj = UserSlots_stubs.create_UserSlots () 
let layout = Creators.create_QVBoxLayout_1 w1 

let button = Creators.create_QPushButton_1 "press me" (Some w1)
let () = button#setCheckable true
let cbox = Creators.create_QCheckBox_1 "check box" (Some w1)

let () = ( 
  let b = (button :> Classes.qWidget) in
  layout#addWidget b
)
let () = (
  let c = (cbox :> Classes.qWidget) in      
  layout#addWidget c
)

let () = Stub_helpers.connect button button#signal_toggled user_obj user_obj#slot_onclicked
let () = Stub_helpers.connect button button#signal_toggled cbox cbox#slot_setChecked

(* WARNING: in Qt you should add something on a widget and that show this widget
 * in reverse order you'll not see this `something` *)

let () = w1#slot_show#call
let _ = QApplication.exec app

