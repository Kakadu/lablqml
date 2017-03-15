(* read test3.qml for explanation *)
open OUnit
open Lablqml

let test3 () =
  let (app,engine) = create_app_engine Sys.argv "src_tests/test3.qml" in
  ignore app;
  let _ =
    (* function for finding roots by name from `engine` *)
    let root = QQmlAppEngine.root_named engine in (* this function raises Failure if object is missing *)
    (* find object named "test" *)
    let test_root = root "test" in

    (* to access a nested QtObject with select it via it's property name *)
    let nested = object_property_named test_root "nested" in

    (* this should cause an exception, as "nonExistent" doens't exist *)
    let non_existent =
      try Some (object_property_named test_root "nonExistent")
      with Failure str -> prerr_endline @@ "NonExistent -- " ^ str; None
    in
    assert (non_existent = None);

    (* now bind to the nested QtObject's property "nested_msg" *)
    let nested_binding = Property.binding nested (fun _ -> ()) in

    (* prove correct access by printing the value *)
    print_endline ("nested message:" ^ (match Property.value nested_binding with `string s -> s | _ -> ""));

    (* create a binding to object "mirror" inside parent "mirror" so we can write to it with test_bind *)
    let mirror_root = root "mirror" in
    let mirror = object_child_named mirror_root "mirror" in

    (* this should cause an exception, as "nonExistent" doens't exist *)
    let non_existent =
      try Some (object_child_named mirror_root "nonExistent")
      with Failure str -> prerr_endline @@ "NonExistent -- " ^ str; None
    in
    assert (non_existent = None);

    let mirror_binding = Property.binding mirror (fun _ -> ()) in

    (* demonstrate binding callback *)
    let test_bind =
      let mirror_value = function
        | `string s ->
           ignore @@ Property.write mirror_binding (`string (s ^ " back"));
           print_endline ("mirror now is:" ^ (match Property.value mirror_binding with `string s -> s | _ -> ""));
           print_endline ("received:" ^ s)
        | _ -> prerr_string "unexpected variant"
      in
      (* When msg of "test" object is changed, update nested "mirror"'s msg to that value *)
      Property.binding test_root mirror_value
    in
    Gc.full_major ();
    QGuiApplication.exec app;
    [test_bind] (* TODO: change *.exec to take list of bindings *)
  in
  print_endline "exited"

let () = test3 ()
