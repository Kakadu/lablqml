open Ppxlib

let () =
  let open Ppx_qt_rewriter.PpxQtCfg in

  Driver.add_arg "-nocpp"
    (Unit (fun () -> config.gencpp <- false)) ~doc:" Don't generate C++";
  Driver.add_arg "-cpp"
    (Unit (fun () -> config.gencpp <- true))  ~doc:" Do    generate C++ (default)";
  Driver.add_arg "-destdir"
    (String (fun s -> config.destdir <- s)) ~doc:"DIR Where to put files";
  Driver.add_arg "-ext"
    (String (fun s -> config.ext <- s)) ~doc:"EXT File extension to use (usually .cpp or .c)";
  Driver.add_arg "-nolocks"
    (Unit (fun () -> config.insert_locks <- false)) ~doc:" omit caml_leave_blocking_section and others";
  ()

let () =
  Driver.standalone ()
