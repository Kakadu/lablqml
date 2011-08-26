open Qtstubs
open Classes
open Stubs

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |] 
 (* HACK to avoid bug with optimization levels *)


let root_widget = make_root_widget () |> new qWidget 
let () = print_endline "root_widget created"

let _ = root_widget#slot_show#call 
let _ = QApplication.exec app







