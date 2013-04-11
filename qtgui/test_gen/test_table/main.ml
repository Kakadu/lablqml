open Qtstubs
open Stub_helpers
open Classes
open Stubs
open Creators

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |];; 
 (* HACK to avoid bug with optimization levels *)
(*
let _ = create_QObject_0' None;;

print_endline "creating normal QWidget";;
let w = create_QWidget_0' None `Window;;
print_endline "normal QWidget created";;

let () = (new qWidget w)#slot_show#call ;;
*)
let root = new qWidget (create_QWidget_0' None `Window) ;;
let table = create_QTableWidget_0 5 2 (Some root);;
let () = root#show ;;
let _ = QApplication.exec app

