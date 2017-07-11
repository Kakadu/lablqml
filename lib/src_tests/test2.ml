open Lablqml

let test2 () =
  let (app,engine) = create_qapplication Sys.argv in
  let w = loadQml "src_tests/test2.qml" engine in
  assert (w <> None);
  let w = match w with Some w -> w | None -> failwith "can't create window" in
  let map1 = PropMap.create () in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"propMap1"
    (PropMap.handler map1);

  let naive_values =
    [ ("title",   QVariant.of_string "hello")
    ; ("count",   QVariant.of_int 0)
    ; ("isSmth",  QVariant.of_bool true)
    ; ("floatVal",QVariant.of_float 1.0)
    ]
  in
  let (set_naive, check_naive) =
    List.fold_left
      (fun (accs,accc) (name, v) ->
         (fun () -> accs (); PropMap.insert map1 ~name v),
         (fun () -> accc (); print_endline name; assert (PropMap.value_ map1 name = v) )
      )
      ( (fun ()->()), fun ()->() )
      naive_values
  in
  set_naive();
  check_naive();

  let title_f,title_v = ("title",QVariant.of_string "hello") in
  let count_f,count_v = ("count",QVariant.of_int 0) in

  PropMap.insert map1 ~name:title_f title_v;
  PropMap.insert map1 ~name:count_f count_v;
  assert (PropMap.value_ map1 title_f = title_v);
  assert (PropMap.value_ map1 count_f = count_v);

  ignore w;
  ignore app;
  (* QQuickWindow.showMaximized w; *)
  (* QGuiApplication.exec app *)
  ()

let () = test2 ()
