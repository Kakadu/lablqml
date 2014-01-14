open Printf
open QmlContext

let () = Printexc.record_backtrace true
external create_myQObject : unit -> cppobj = "caml_create_myobject"
external caml_add_slot: cppobj -> string -> string -> unit = "caml_addSlot"
external caml_store: cppobj -> < .. > -> unit = "caml_storeCamlObject"


let main () =
  let cppo = create_myQObject () in
  let _o1 = object(self)
    initializer begin
      caml_store cppo self;
      let name = "slot1" in
      let sign = "void(QString)" in
      caml_add_slot cppo name sign
    end
    method slot1 msg = Printf.printf "message is: '%s'\n%!" msg
  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" cppo;

  ()

let () =
  Callback.register "doCaml" main
