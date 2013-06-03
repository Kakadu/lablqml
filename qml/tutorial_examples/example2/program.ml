open QmlContext

let main () =
  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    inherit Controller.base_Controller controller_cppobj as super
    method onMouseClicked msg = Printf.printf "OCaml says: '%s'\n%!" msg
  end in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler;
  print_endline "startup initialiation at OCaml side"

let () = Callback.register "doCamlInitialization" main
