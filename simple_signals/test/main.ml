open Qt
open QtGui
open QPushButton

(*let app = QApplication.create Sys.argv in *)
let app = createApp Sys.argv in
let w  = QPushButton.createButton "clickme" "clickme" in 
print_endline "here";
(*
w#setCheckable true;
w#setChecked true;
*)

connect w (w#clicked) app (app#quit);

w#show ();
print_endline "app.exec";
let _ = app#exec () in ()


