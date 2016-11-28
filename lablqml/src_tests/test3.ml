open OUnit
open QmlContext

let test3 () =
  let (app,engine) = create_app_engine Sys.argv "src_tests/test3.qml" in
  let qobj1_option = Binding.object_of_name engine "test" in
  match qobj1_option with
  | Some o ->
     let bind1 = Binding.qml_property ~f:(fun _ -> ()) o "msg" in
     ignore app;
     ()
  | None -> prerr_string "object not found"

let () = test3 ()
