open Printf
open Lablqml

let () = Printexc.record_backtrace true


let _ =
  run_with_QQmlApplicationEngine Sys.argv (fun () -> ()) "qrc:///ui/Root.qml"

