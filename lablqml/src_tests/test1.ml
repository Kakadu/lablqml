open OUnit
open QmlContext

let () = print_endline "AAA"


let test1 () =
  let (app,engine) = create_qapplication Sys.argv in
  let w = loadQml "test1.qml" engine in
  assert (w <> None);
  let w = match w with Some w -> w | None -> failwith "can't create window" in
  QQuickWindow.showMaximized w;
  QGuiApplication.exec app
