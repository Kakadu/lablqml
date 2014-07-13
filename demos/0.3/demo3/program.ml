open QmlContext

let main () =
  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    inherit Controller.controller controller_cppobj as super

    val mutable counter = 0
    method getclicksCount () = counter

    method onMouseClicked () =
      counter <- counter+1;
      self#emit_clicksCountChanged counter;
      (* Send signal with a new message *)
      self#emit_hiGotten (Printf.sprintf "Hi %d times" counter);
  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () =
  run_with_QQmlApplicationEngine Sys.argv main "Root.qml"


