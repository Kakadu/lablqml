open Printf
open Lablqml

let () = Printexc.record_backtrace true

class myitem cppObj = object(self)
  inherit Item.item cppObj as super
  method getname () = "Vasya"
  method getage () = 11
end

let main () =
  let cppObj = Item.create_item () in
  let item = new myitem cppObj in

  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    inherit Controller.controller controller_cppobj as super
    method getobj () = QVariant.of_object item#handler
    method getperson () = self#getobj ()
    method setobj  = function
      | `qobject o ->
         let item = new myitem o in
         printf "qobject: %s %d\n%!" (item#getname ()) (item#getage())
      | `string s -> printf "String: '%s'\n%!" s
      | `int  x -> printf "int %d\n%!" x
      | `float f -> printf "float %f\n%!" f
      | `bool b -> printf "bool %b\n%!" b
      | `empty -> print_endline "empty"
  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () = Callback.register "doCaml" main
