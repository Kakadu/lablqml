open Classes

let app = create_app [|  "1";"2";"3";"4";"5";"6";"7" |];; 
(*
let b = new b
let () = print_endline "trying to call b#foo"
let () = b#foo ()
let () = print_endline "trying to call b#boo"
let () = b#boo ()

*)

let aa = new qwidget (create_qwidget' None)
(*
let () = print_endline "trying to call keyPressEvent"
let () = aa#keyPressEvent (new qKeyEvent (simpleQKeyEvent ()))

let () = Printf.printf "(Obj.magic aa#keyPressEvent) =  %i\n%!" ((Obj.magic aa#keyPressEvent) )
let () = Printf.printf 
  "(Obj.tag (Obj.repr aa#keyPressEvent)) =  %i\n%!" (Obj.tag (Obj.repr aa#keyPressEvent))
*)

let () = aa#show;;

let _ =  exec ()
