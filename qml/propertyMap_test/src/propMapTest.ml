open QmlContext

let () = Printexc.record_backtrace true

let main () =
  let callback name _ =
    Printf.printf "property '%s' has been changed\n%!" name;
    ()
  in
  let map = PropMap.create ~callback () in
  PropMap.insert map ~name:"title" (QVariant.of_string "hello");
  PropMap.insert map ~name:"count" (QVariant.of_int 0);
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"propMap1" (PropMap.handler map);

  let (_: Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      PropMap.insert map ~name:"count" (QVariant.of_int !counter);
      f ()
    in
    Thread.create f ()
  in
  ()

let _ =
  run_with_QQmlApplicationEngine Sys.argv main "qrc:///ui/Root.qml"
