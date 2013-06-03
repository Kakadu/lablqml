let main () = print_endline "startup initialiation at OCaml side"
let () = Callback.register "doCamlInitialization" main