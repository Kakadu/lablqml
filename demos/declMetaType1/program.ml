open Lablqml

class demo_class_impl cppobj =
  let counter = ref 0 in
  object(self)
    inherit DemoClass1.demoClass1 cppobj
    method getcount () = !counter
    method tick () =
      incr counter;
      print_endline "Emitting a signal from OCaml to QML";
      self#emit_countChanged (10 * !counter)
  end

let main () =
  DemoClass1.register_metatype_demoClass1 (new demo_class_impl)
    ~ns:"com.asdf" ~classname:"DemoClass1" ~major:1 ~minor:0;
  print_endline "startup initialiation at OCaml side"

let () = run_with_QQmlApplicationEngine Sys.argv main "Root.qml"
