open Qt
open QtGui
open QPushButton

(*let app = QApplication.create Sys.argv in *)
let app = createApp Sys.argv in
let w = QPushButton.createWidget () in
let b1 = QPushButton.createButton "button1" "button1" in 
(*let b2 = QPushButton.createButton "button2" "button2" in 
let lay = QPushButton.createVBoxLayout None in
lay#addWidget b1;
lay#addWidget b2;
*)
print_endline "here";

connect b1 (b1#clicked) app (app#quit);

w#show ();
print_endline "app.exec";
let _ = app#exec () in ()


