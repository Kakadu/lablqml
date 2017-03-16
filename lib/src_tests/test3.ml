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
    let test_binding = OCamlObject.binding test_root "mlvalue" (fun _ -> ()) in

    ignore @@ OCamlObject.write test_binding (`string "Hello QML from OCaml!");

    (* to access a nested QtObject with select it via it's property name *)
    let nested = object_property_named test_root "nested" in

    (* this should cause an exception, as "nonExistent" doens't exist *)
    let non_existent =
      try Some (object_property_named test_root "nonExistent")
      with Failure str -> None
    in
    assert (non_existent = None);

    (* now bind to the nested QtObject's property "nested_msg" *)
    let nested_binding = OCamlObject.binding nested "mlvalue" (fun _ -> ()) in

    (* create a binding to object "mirror" inside parent "mirror" so we can write to it with test_bind *)
    let mirror_root = root "mirror" in
    let mirror = object_child_named mirror_root "mirror" in

    (* this should cause an exception, as "nonExistent" doens't exist *)
    let non_existent =
      try Some (object_child_named mirror_root "nonExistent")
      with Failure str -> None
    in
    assert (non_existent = None);

    let mirror_binding = OCamlObject.binding mirror "mlvalue" (fun _ -> ()) in

    (* demonstrate binding callback *)
    let test_bind =
      let mirror_value = function
        | `string s ->
           ignore @@ OCamlObject.write mirror_binding (`string (s ^ " back"));
           print_endline ("received:" ^ s)
        | _ -> prerr_string "unexpected variant"
      in
      (* When msg of "test" object is changed, update nested "mirror"'s msg to that value *)
      OCamlObject.binding test_root "mlvalue" mirror_value
    in
    Gc.full_major ();
    QGuiApplication.exec app;
    [test_bind] (* TODO: change *.exec to take list of bindings *)
  in
  print_endline "exited"

let () = test3 ()
