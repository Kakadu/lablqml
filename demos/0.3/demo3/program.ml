open Printf

let () = Printexc.record_backtrace true

open QmlContext

let main () =
  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    inherit Controller.controller controller_cppobj as super
    val mutable _x = 0
    method onMouseClicked () =
      print_endline "OCaml says: Mouse Clicked!";
      _x <- _x+1;
      self#emit_hiGotten (sprintf "Hi %d times" _x);
      self#emit_clicksCountChanged _x
    method getclicksCount () = _x
  end in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () =
  run_with_QQmlApplicationEngine Sys.argv main "Root.qml"


