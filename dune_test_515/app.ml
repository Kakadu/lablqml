open Lablqml
open Format

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

let lwt_thread () =
  let open Lwt in
  let rec fake_worker () = Lwt_unix.sleep 1.0 >>= fun () -> fake_worker () in
  Thread.create (fun () -> Lwt_main.run (fake_worker ())) () |> ignore

let old_style () =
  let controller_cppobj = MyControls.create_myslider () in
  let controller =
    object (self)
      inherit MyControls.myslider controller_cppobj

      val mutable desc = ""

      method updateDescription info =
        if info <> desc then (
          desc <- info;
          self#emit_descrChanged desc)

      method getdescr () = desc
    end
  in
  Lablqml.set_context_property
    ~ctx:(get_view_exn ~name:"rootContext")
    ~name:"myslider1" controller#handler;
  ()

let _ =
  run_with_QQmlApplicationEngine Sys.argv
    (fun () ->
      lwt_thread ();
      old_style();
      create_threads ())
    "qrc:///ui/Root.qml"
