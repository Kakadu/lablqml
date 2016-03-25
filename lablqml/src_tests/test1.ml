open OUnit
open QmlContext


let test1 () =
  let (app,engine) = create_qapplication Sys.argv in
  let w = loadQml "src_tests/test1.qml" engine in
  assert (w <> None);
  let w = match w with Some w -> w | None -> failwith "can't create window" in

  assert ((QQuickWindow.as_test_object w)#property "intProp" = QVariant.of_int 123);
  ()


let () = test1 ()
