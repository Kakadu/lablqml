open Printf

let () = Printexc.record_backtrace true

open QmlContext

let main () =

  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    val mutable _x = 0
    val mutable _y = 0
    val mutable _state = "state1"
    inherit Controller.base_Controller controller_cppobj as super
    method onMouseClicked () = 
      print_endline "Mouse Clicked!";
      self#emit_ebanashka (sprintf "Hi %d times" (Random.int 100))

  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () = Callback.register "doCaml" main

