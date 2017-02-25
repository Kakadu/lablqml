open Lablqml

class demo_class_impl =
  let counter = ref 0 in
  object
  inherit DemoClass1.demoClass1 ()
  method getcount () = !counter
  method tick () = incr counter
end

let main () =
  DemoClass1.register_metatype_demoClass1 (fun () -> new demo_class_impl)
    ~ns:"com.asdf" ~classname:"DemoClass1" ~major:1 ~minor:0;
  print_endline "startup initialiation at OCaml side"

let () = run_with_QQmlApplicationEngine Sys.argv main "Root.qml"
