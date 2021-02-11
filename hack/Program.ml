open Printf
open Lablqml

let () = Printexc.record_backtrace true

(* class type virtual controller_t =
  object
    inherit Controller.controller

    method updateDescription : string -> unit
  end

type options = { mutable ctrl : controller_t }

let options = { ctrl = Obj.magic (object end) }
 *)
let main () = ()

(* let controller_cppobj = Controller.create_controller () in
  let controller =
    object (self)
      inherit Controller.controller controller_cppobj

      val mutable desc = ""

      method updateDescription info =
        if info <> desc
        then (
          desc <- info;
          self#emit_descrChanged desc)

      method getdescr () = desc
    end
  in
  options.ctrl <- controller;
  set_context_property
    ~ctx:(get_view_exn ~name:"rootContext")
    ~name:"controller"
    controller#handler *)

let create_threads () =
  let (_ : Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      Format.printf "Tick\n%!";
      MyControls.MySingleton.doSomething ();
      MyControls.MySingleton.setSomeProperty !counter;
      f ()
    in
    Thread.create f ()
  in
  ()
;;

let _ =
  (* Lwt_main.run @@ join *)
  (*   [ loop "asdf" *)
  (*   ; main_thread () *)
  (*   (\* ; thread2() *\) *)
  (*   ] *)
  run_with_QQmlApplicationEngine
    Sys.argv
    (fun () ->
      main ();
      create_threads ())
    "qrc:///ui/Root.qml"
;;
