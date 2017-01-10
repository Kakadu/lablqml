(* read test3.qml for explanation *)
open OUnit
open QmlContext

let test3 () =
  let (app,engine) = create_app_engine Sys.argv "src_tests/test3.qml" in
  ignore app;
  let _ =
    let root = QQmlAppEngine.root_named engine in (* this function raises Failure if object is missing *)
    let test = root "test" in
    let mirror_root = root "mirror" in
    let mirror = object_child_named mirror_root "mirror" in

    let mirror_bind = Property.bind_variant mirror "text" (fun _ -> ()) in
    let test_bind =
      let mirror_value = function
        | `string s ->
           ignore @@ Property.write mirror_bind (`string s);
           print_endline ("mirror now is:" ^ (match Property.value mirror_bind with `string s -> s | _ -> ""));
           print_endline ("received:" ^ s)
        | _ -> prerr_string "unexpected variant"
      in
      Property.bind_variant test "msg" mirror_value
    in
    Gc.full_major ();
    QGuiApplication.exec app;
    [test_bind] (* TODO: change *.exec to take list of bindings *)
  in
  print_endline "exited"

let () = test3 ()
