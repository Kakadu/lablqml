open Qtstubs
open Stub_helpers
open Classes
open Stubs
open Creators

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |];; 
 (* HACK to avoid bug with optimization levels *)

print_endline "creating normal QWidget";;
let w = create_QWidget_0' None `Window;;
print_endline "normal QWidget created";;

class mywidget me = object (self)
  inherit qWidget me as super
  method! keyPressEvent event = 
          Printf.printf "catching keyPressEvent\n";
          flush stdout
end;;

let myobj = new mywidget w;;
let () = myobj#slot_show#call ;;
let _ = QApplication.exec app







