open QmlContext

(* Helper function to expose the object 'obj' into QtQuick engine.
 * Argument 'name' means the name for this object in QtQuick
 *
 * It is not interesting function. You can simply copy and paste it.
 *)
let expose ~name obj =
  (* 'obj#handler' is raw C++ pointer to object. *)
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name obj#handler

(* This function is called once before constructing window. It exposes some
 * objects from OCaml to QtQuick engine
 *)
let init: unit -> unit = fun () ->
  let controller = object(self)
    inherit Controller.controller (Controller.create_controller ()) as super
    (* 'onMouseClicked' is the method marked with [@@qtmeth] attribute in 'controller.ml'
     * We need to implement it. *)
    method onMouseClicked () =
      print_endline "OCaml says: Mouse Clicked!"
  end in
  (* Once object is created we make it available in QtQuick engine *)
  expose ~name:"controller" controller

let () =
  let qmlfile = "Root.qml" in
  (* We start application and pass initialization function and main QML file *)
  run_with_QQmlApplicationEngine Sys.argv init qmlfile


