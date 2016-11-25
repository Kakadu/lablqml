open OUnit
open QmlContext

let test1 () =
  let (app,appEngine) = create_app_engine Sys.argv "src_tests/test4.qml" in

  let single = SingleFunc.create
      (fun () -> Printf.printf "single func in OCaml\n%!")
  in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"runner"
    (SingleFunc.handler single);

  QGuiApplication.exec app;
  ()


let () = test1 ()
