open Printf

let () = Printexc.record_backtrace true

open QmlContext

let main () =
  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    val mutable _x = 0
    val mutable _y = 0
    val mutable _state = "state1"
    inherit Controller.base_Controller controller_cppobj as super
    method onMouseClicked () = self#setX (_x+1); print_endline "Mouse Clicked!"
    method y () = _y
    method x () = _x
    method state () = _state
    method setX v =
      if v<>_x then ( _x<-v; self#emit_xChanged _x )
    method setY v =
      if v<>_y then ( _y<-v; self#emit_yChanged _y )
    method setState v =
      if v<>_state then ( _state <- v; self#emit_stateChanged _state )

  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler

let () = Callback.register "doCaml" main
