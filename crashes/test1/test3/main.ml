open Qtstubs
open Classes
open Stubs
open Stub_helpers

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)

let w1 = Creators.create_QWidget_0 None `Window
let w2 = Creators.create_QWidget_0 None `Window

(* WARNING: in Qt you should add something on a widget and that show this widget
 * in reverse order you'll not see this `something` *)

let s1 = Creators.create_QScrollBar_1 `Horizontal (Some w1)
let s1 = Creators.create_QScrollBar_1 `Vertical   (Some w2)


let () = w1#slot_show#call 
let () = w2#slot_show#call




let _ = QApplication.exec app







