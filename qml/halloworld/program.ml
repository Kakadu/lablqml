open Printf

let () = Printexc.record_backtrace true

open QmlContext

let main () =
  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    inherit Controller.base_Controller controller_cppobj as super
    method onMouseClicked () = print_endline "Mouse Clicked!"
  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () = Callback.register "doCaml" main
