open QmlContext

let init () =
  let controller = object(self)
    inherit Controller.controller (Controller.create_controller ()) as super
    method onMouseClicked () = print_endline "OCaml says: Mouse Clicked!"
  end in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () =
  run_with_QQmlApplicationEngine Sys.argv init "Root.qml"


