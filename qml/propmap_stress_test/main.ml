open QmlContext

let qml_mapper () =
(*  let i_channel = Unix.in_channel_of_descr socket in
  let o_channel = Unix.out_channel_of_descr socket in *)

  let value_changed name value = match value with
    | `int i -> Printf.printf "%d" i; flush Pervasives.stdout;
    | _ -> ()
  in
  let alpha = PropMap.create ~callback:value_changed () in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"alpha" (PropMap.handler alpha);
  PropMap.insert alpha ~name:"count" (QVariant.of_int 0);

  let callback _ _ = () in
  let beta = PropMap.create ~callback () in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"beta" (PropMap.handler beta);

  let rec f x =
    PropMap.insert beta ~name:"count" (QVariant.of_int x);
    Thread.delay 0.01;
    f (x+1)
  in  
  let alpha_thread = Thread.create f 0 in
  Printf.printf "beta thread: %d\n" (Thread.id alpha_thread);
  flush Pervasives.stdout;

  ()

let () = run_with_QQmlApplicationEngine Sys.argv (fun () -> qml_mapper ()) "ui.qml"
