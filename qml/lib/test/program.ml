open QmlContext

let main () =
  print_endline "Do Something";
  let data = ["A";"B";"C";"D"] in
  let open A in
  let (model: typ_for_A) = object(self)
    method sizey _ = 0
    method parent _ = QModelIndex.empty
    method index row column parent =
      if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
      else QModelIndex.empty
    method columnCount _ =
      print_endline "columnCount";
      1
    method rowCount _ : int =
      List.length data

    method hasChildren _ = self#rowCount QModelIndex.empty > 0
    method data index role =
      print_endline "data"; flush stdout;
      let r = QModelIndex.row index in
      if (r<0 || r>= List.length data) then QVariant.empty
      else begin
        if (role=0 || role=555) (* DisplayRole *)
        then QVariant.of_string (List.nth data r)
        else QVariant.empty
      end

  end in
  let cpp_model = create_A model in
  add_role cpp_model 555 "homm";
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"myModel" cpp_model
;;
let () = Callback.register "doCaml" main
