open Lablqml
open Format

let () = Printexc.record_backtrace true

module X = struct
  class type virtual controller_t =
    object
      inherit MyControls.myslider

      method updateDescription : string -> unit
    end

  type options = { mutable ctrl : controller_t }

  let options = { ctrl = Obj.magic (object end) }

  let main () =
    let controller_cppobj = MyControls.create_myslider () in
    let controller =
      object (self)
        inherit MyControls.myslider controller_cppobj

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
      controller#handler
  ;;
end

let create_threads () =
  let (_ : Thread.t) =
    let counter = ref 0 in
    let rec f () =
      let _ = Thread.delay 1.0 in
      incr counter;
      Format.printf "Tick\n%!";
      MyControls.MySingleton.setSomeProperty !counter;
      X.options.ctrl#updateDescription (sprintf "from old style object: %d" !counter);
      f ()
    in
    Thread.create f ()
  in
  ()
;;

let _ =
  run_with_QQmlApplicationEngine
    Sys.argv
    (fun () ->
      X.main ();
      create_threads ())
    "qrc:///ui/Root.qml"
;;
