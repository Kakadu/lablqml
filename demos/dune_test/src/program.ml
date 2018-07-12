open Printf
open Lablqml

let () = Printexc.record_backtrace true

class type virtual controller_t = object
  inherit Controller.controller
  method updateDescription: string -> unit
end

type options = { mutable ctrl: controller_t }

let options = { ctrl=Obj.magic (object end) }

let main () =
  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    inherit Controller.controller controller_cppobj as super

    val mutable desc = ""
    method updateDescription info =
      if info <> desc then begin
        desc <- info;
        self#emit_descrChanged desc
      end

    method getdescr () = desc
  end in

  options.ctrl <- controller;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

(*
open Lwt

let main_thread () =
  Lwt_io.printf "Main thread\n%!" >>= (fun _ -> run_with_QQmlApplicationEngine Sys.argv main "qrc:///ui/Root.qml"; Lwt.return ())

let rec thread2 () =
  Lwt_io.printf "Second thread\n%!" >>= (fun () -> Lwt_unix.sleep 5.0) >>= (fun () -> thread2  ())

let rec thread3 () =
  Lwt_io.printf "Third thread\n%!" >>= (fun () -> Lwt_unix.sleep 5.0) >>= (fun () -> thread3 ())

let rec loop s =
  Lwt_unix.sleep 1. >>= fun () ->
  Format.printf "Hello %s@." s;
  loop s
 *)

let create_threads () =
  let (_: Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      options.ctrl#updateDescription (Printf.sprintf "ticked %d times" !counter);
      f ()
    in
    Thread.create f ()
  in
  ()


let _ =
  (* Lwt_main.run @@ join *)
  (*   [ loop "asdf" *)
  (*   ; main_thread () *)
  (*   (\* ; thread2() *\) *)
  (*   ] *)
  run_with_QQmlApplicationEngine Sys.argv (fun() -> main (); create_threads ()) "qrc:///ui/Root.qml"
