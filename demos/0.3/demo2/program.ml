open QmlContext

let expose ~name obj =
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name obj#handler

let init () =
  let controller = object(self)
    inherit Controller.controller (Controller.create_controller ()) as super

    val mutable counter = 0
    method getclicksCount () = counter

    method onMouseClicked () =
      print_endline "OCaml says: Mouse Clicked!";
      counter <- counter+1;
      self#emit_clicksCountChanged counter
  end in

  expose ~name:"controller" controller

let () =
  run_with_QQmlApplicationEngine Sys.argv init "Root.qml"


