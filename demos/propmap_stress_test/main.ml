open Lablqml

let value_changed name value = match value with
    | `int i -> if i mod 1000 = 0 then (Printf.printf "%s %d;\n" name i; flush Pervasives.stdout;)
    | _ -> ()

let alpha = PropMap.create ~callback:value_changed ()
let callback _ _ = ()
let beta = PropMap.create ~callback ()

let qml_mapper () =
(*  let i_channel = Unix.in_channel_of_descr socket in
  let o_channel = Unix.out_channel_of_descr socket in *)

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"alpha" (PropMap.handler alpha);
  PropMap.insert alpha ~name:"countA" (QVariant.of_int 0);
  PropMap.insert alpha ~name:"c0" (QVariant.of_int 0);
  PropMap.insert alpha ~name:"c1" (QVariant.of_int 0);

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"beta" (PropMap.handler beta);

  let rec f x =
    PropMap.insert beta ~name:"count" (QVariant.of_int x);
    Thread.delay 0.00001;
    f (x+1)
  in  
  ignore (Thread.create f 0)

let () = run_with_QQmlApplicationEngine Sys.argv (fun () -> qml_mapper ()) "ui.qml"
