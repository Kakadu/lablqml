open QmlContext

let () = Random.init 147
let () = Printexc.record_backtrace true

let www = ref [||]

let main () =
  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    inherit Controller.controller controller_cppobj as super

    method foo1 () =
      Thread.delay @@ Random.float 0.5;
      www := Array.init 100 (fun _ -> 100);
      Array.map ((+)1) !www;
      print_endline "foo1"
    method foo2 () =
      Thread.delay @@ Random.float 0.5;
      let xxx = Array.init 100 (fun _ -> 100) in
      www := Array.map ((+)1) xxx;
      print_endline "foo2"

  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext")
    ~name:"controller" controller#handler;



  let callback name _ =
    Printf.printf "property '%s' has been changed\n%!" name;
    ()
  in
  let map = PropMap.create ~callback () in
  PropMap.insert map ~name:"title" (QVariant.of_string "hello");
  PropMap.insert map ~name:"count" (QVariant.of_int 0);
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"propMap1" (PropMap.handler map);

  for i=1 to 100 do
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
  done

let _ =
  run_with_QQmlApplicationEngine Sys.argv main "qrc:///ui/Root.qml"
