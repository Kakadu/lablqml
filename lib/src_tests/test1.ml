open Lablqml

let test1 () =
  let (app,engine) = create_qapplication Sys.argv in

  let single = SingleFunc.create
      (fun () -> Printf.printf "single func in OCaml\n%!")
  in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"runner"
    (SingleFunc.handler single);

  let w = loadQml "src_tests/test1.qml" engine in
  assert (w <> None);
  let _w = match w with Some w -> w | None -> failwith "can't create window" in

  QGuiApplication.exec app;
  ()

let () = test1 ()
