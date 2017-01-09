open Printf

let () = Printexc.record_backtrace true

open Lablqml

class virtual abstractListModel cppobj = object(self)
  inherit IntModel.intModel cppobj as super
  method parent _ = QModelIndex.empty
  method index row column parent =
    if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
    else QModelIndex.empty
  method columnCount _ = 1
  method hasChildren _ = self#rowCount QModelIndex.empty > 0
end

let main () =
  let cpp_model = IntModel.create_intModel () in
  IntModel.add_role cpp_model 555 "cellX";
  IntModel.add_role cpp_model 556 "title";
  IntModel.add_role cpp_model 666 "obj";

  let data = List.map (fun n ->
    let cppObj = DataItem.create_dataItem () in
      object(self)
        inherit DataItem.dataItem cppObj as super
        method getcellX () = n
        val mutable text_ = sprintf "text %d" n
        method gettext  () = text_
        method setText s =
          if (s <> self#gettext ()) then ( text_ <- s; self#emit_textChanged s);

      end
  ) [1;2;3] in

  let model = object(self)
    inherit abstractListModel cpp_model as super
    method rowCount _ = 3
    method data index role =
      let n = QModelIndex.row index in
      if (n<0 || n>= List.length data) then QVariant.empty
      else begin
        match role with
        | 0
        | 555 (* DisplayRole *) ->
            QVariant.of_int ((List.nth data n)#getcellX ())
        | 556 (* title *) ->
            QVariant.of_string ((List.nth data n)#gettext ())
        | 666 -> QVariant.of_object ((List.nth data n)#handler)
        | _ -> QVariant.empty
      end
  end in

  let controller_cppobj = Controller.create_controller () in
  let controller = object(self)
    val mutable _x = 0
    val mutable _y = 0
    val mutable _state = "state1"
    inherit Controller.controller controller_cppobj as super
    method onMouseClicked () = self#setX (_x+10); print_endline "Mouse Clicked!"
    method gety () = _y
    method getx () = _x
    method getstate () = _state
    method setX v =
      if v<>_x then ( _x<-v; self#emit_xChanged _x );
      (List.hd data)#setText (sprintf "new %d" v)

    method setY v =
      if v<>_y then ( _y<-v; self#emit_yChanged _y )
    method setState v =
      if v<>_state then ( _state <- v; self#emit_stateChanged _state )

  end in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"intModel"   model#handler

let () = Callback.register "doCaml" main
