open Printf
open Lablqml

let () = Printexc.record_backtrace true

class type virtual controller_t =
  object
    inherit Controller.controller

    method updateDescription : string -> unit
  end

type options = { mutable ctrl : controller_t }

let options = { ctrl = Obj.magic (object end) }

let main () =
  let controller_cppobj = Controller.create_controller () in
  let controller =
    object (self)
      inherit Controller.controller controller_cppobj

      val mutable desc = ""

      method updateDescription info =
        if info <> desc then (
          desc <- info;
          self#emit_descrChanged desc)

      method getdescr () = desc
    end
  in
  options.ctrl <- controller;
  set_context_property
    ~ctx:(get_view_exn ~name:"rootContext")
    ~name:"controller" controller#handler

let create_model () =
  let open Controller in
  let cpp_model = Controller.IntModel.create_c () in
  let cell_role_id = 555 in
  let title_role_id = 556 in
  let obj_role_id = 666 in
  IntModel.add_role cpp_model 555 "cellX";
  IntModel.add_role cpp_model 556 "title";
  IntModel.add_role cpp_model 666 "obj";

  let data =
    List.map
      (fun n ->
        let cpp_obj = DataItem.create_dataItem () in
        object (self)
          inherit DataItem.dataItem cpp_obj

          method getcellX () = n

          val mutable text_ = sprintf "text %d" n

          method gettext () = text_

          method setText s =
            if s <> self#gettext () then (
              text_ <- s;
              self#emit_textChanged s)
        end)
      [ 1; 2; 3 ]
  in
  let module M = struct
    class virtual abstractListModel cppobj =
      object (self)
        inherit IntModel.c cppobj

        method parent _ = QModelIndex.empty

        method index row column parent =
          if row >= 0 && row < self#rowCount parent then
            QModelIndex.make ~row ~column:0
          else QModelIndex.empty

        method columnCount _ = 1

        method hasChildren _ = self#rowCount QModelIndex.empty > 0
      end
  end in
  let model =
    object (self)
      inherit M.abstractListModel cpp_model

      method rowCount _ = 3

      method data index role =
        let n = QModelIndex.row index in
        if n < 0 || n >= List.length data then QVariant.empty
        else
          match role with
          | 0 -> QVariant.of_int ((List.nth data n)#getcellX ())
          | r when r = cell_role_id ->
              QVariant.of_int ((List.nth data n)#getcellX ())
          | r when r = title_role_id ->
              QVariant.of_string ((List.nth data n)#gettext ())
          | r when r = obj_role_id ->
              QVariant.of_object (List.nth data n)#handler
          | _ -> QVariant.empty
    end
  in

  set_context_property
    ~ctx:(get_view_exn ~name:"rootContext")
    ~name:"intModel" model#handler
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
  let (_ : Thread.t) =
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
  run_with_QQmlApplicationEngine Sys.argv
    (fun () ->
      main ();
      create_model ();
      create_threads ())
    "qrc:///ui/Root.qml"
