open Classes

let app = create_app [| (* "1";"2";"3";"4";"5";"6";"7" *) |];; 
(*
let b = new b
let () = print_endline "trying to call b#foo"
let () = b#foo ()
let () = print_endline "trying to call b#boo"
let () = b#boo ()

*)

let aa = new qwidget (create_qwidget' None)
let () = aa#show;;

let _ =  exec app
