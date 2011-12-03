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

class mywidget me = object (self)
  inherit qWidget me as super
  method! keyPressEvent event = 
          Printf.printf "catching keyPressEvent: %d\n" event#key;
          flush stdout
end;;
let w = create_QWidget_twin_0' None `Window 
let () = 
  print_endline "QWidget_twin created";
  let o = new mywidget w in
  o#setWindowTitle "Press some key and look to console";
  o#slot_show#call;
  print_endline "Bingo!"
;; 
let _ = QApplication.exec app

