let () =
  let open Ppx_qt_rewriter.Ppx_qt in

  Ppxlib.Driver.add_arg "-nocpp"
    (Unit (fun () -> config.gencpp <- false)) ~doc:" Don't generate C++";
  Ppxlib.Driver.add_arg "-cpp"
    (Unit (fun () -> config.gencpp <- true))  ~doc:" Do    generate C++ (default)";
  Ppxlib.Driver.add_arg "-destdir"
    (String (fun s -> config.destdir <- s)) ~doc:"DIR Where to put files";
  Ppxlib.Driver.add_arg "-ext"
    (String (fun s -> config.ext <- s)) ~doc:"EXT File extension to use (usually .cpp or .c)";
  Ppxlib.Driver.add_arg "-nolocks"
    (Unit (fun () -> config.insert_locks <- false)) ~doc:"omit caml_leave_blocking_section and others";
  ()

let () =
  Ppxlib.Driver.standalone ()
