open Qtstubs
open Stub_helpers
open Classes
open Stubs
open Creators

let app = QApplication.create [| "1";"2";"3";"4";"5";"6" |];; 
 (* HACK to avoid bug with optimization levels *)

print_endline "creating normal QObject";;
let qobj = create_QObject_0 None;;
print_endline "normal qobject created";;

class myqObject me = object (self)
  inherit qObject me as super
  method! setObjectName name = 
          Printf.printf "setting name of myqObject to %s\n" name;
          flush stdout;
          super#setObjectName name
end;;

print_endline "creating my qObject";;
let myqobj = create_QObject_0' None |> new myqObject;;
print_endline "setting name of my qobject";;
let () = myqobj#setObjectName "somename";;
let _ = QApplication.exec app







