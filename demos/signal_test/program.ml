open Printf

let () = Printexc.record_backtrace true
let root_qml_file = "Root.qml"

open Lablqml

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
  let app,eng = create_qapplication Sys.argv in
  QQmlEngine.register_context "rootContext" eng;
  QQmlEngine.add_import_path "somedir" eng;
  main ();
  match loadQml root_qml_file eng with
  | None -> failwith "can't load QML"
  | Some w ->
      QQuickWindow.showMaximized w;
      QGuiApplication.exec app
