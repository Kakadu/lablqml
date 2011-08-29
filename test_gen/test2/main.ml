open Qtstubs
open Classes
open Stubs

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)


let root_widget = make_root_widget () |> new qWidget 
let () = print_endline "root_widget created"


let btn1 = Creators.create_QPushButton_0 "Exit Button 1" None 

let btn2 = Creators.create_QPushButton_0 "Exit Button 2" (Some root_widget) 
 
let () = btn1#slot_show#call

let _ = root_widget#slot_show#call 

(* WARNING: in Qt you should add something on a widget and that show this widget
 * in reverse order you'll not see this `something` *)





let _ = QApplication.exec app







