open Lablqml

let () = Printexc.record_backtrace true

let create_threads () =
  let (_ : Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      Format.printf "Tick\n%!";
      MyControls.MySingleton.setSomeProperty !counter;
      f ()
    in
    Thread.create f ()
  in
  ()
;;

let _ =
  run_with_QQmlApplicationEngine
    Sys.argv
    (fun () -> create_threads ())
    "qrc:///ui/Root.qml"
;;
