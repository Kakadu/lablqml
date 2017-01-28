open Lablqml

let value_changed name value = match value with
    | `int i -> (Printf.printf "%s %d;\n%!" name i)
    | _ -> ()

let beta = PropMap.create ()
let alpha = PropMap.create ~callback:value_changed ()

let qml_mapper () =
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"alpha" (PropMap.handler alpha);
  PropMap.insert alpha ~name:"countA" (QVariant.of_int 0);
  PropMap.insert alpha ~name:"c0" (QVariant.of_int 0);
  PropMap.insert alpha ~name:"c1" (QVariant.of_int 0);

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"beta" (PropMap.handler beta);

  let rec f x =
    PropMap.insert beta ~name:"countB" (QVariant.of_int x);
    Thread.delay 0.0001;
    f (x+1)
  in
  ignore (Thread.create f 0)

let () = run_with_QQmlApplicationEngine Sys.argv (fun () -> qml_mapper ()) "ui.qml"
