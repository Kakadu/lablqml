let main () =
  print_endline "Do Something"

;;
let () = Callback.register "doCaml" main
