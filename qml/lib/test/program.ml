open QmlContext
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let main () =
  print_endline "Do something";
  let data = [["A"];["B";"C"]; ["D";"E";"F"]]
    |> List.map (List.map (fun str ->
      let open DataItem in
      let (item: typ_for_DataItem) = object
        method name () = str
        method sort () = String.lowercase str
      end in
      create_DataItem item
    ))
    |> List.map (fun data ->
      let open MiniModel in
      let (model: typ_for_MiniModel) = object(self)
        method parent _ = QModelIndex.empty
        method index row column parent =
          if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
          else QModelIndex.empty
        method columnCount _ = 1
        method rowCount _ = List.length data
        method hasChildren _ = self#rowCount QModelIndex.empty > 0
        method data index role =
          let r = QModelIndex.row index in
          if (r<0 || r>= List.length data) then QVariant.empty
          else begin
            if (role=0 || role=555) (* DisplayRole *)
            then QVariant.of_object (List.nth data r)
            else QVariant.empty
          end
      end in
      let cpp_model = create_MiniModel model in
      add_role cpp_model 555 "qwe";
      cpp_model
    ) in

  let open MainModel in
  let (model: typ_for_MainModel) = object(self)
    method parent _ = QModelIndex.empty
    method index row column parent =
      if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
      else QModelIndex.empty
    method columnCount _ = 1
    method rowCount _ = List.length data
    method hasChildren _ = self#rowCount QModelIndex.empty > 0
    method data index role =
      let r = QModelIndex.row index in
      if (r<0 || r>= List.length data) then QVariant.empty
      else begin
        if (role=0 || role=555) (* DisplayRole *)
        then QVariant.of_object (List.nth data r)
        (*then QVariant.of_string "XXX"*)
        else QVariant.empty
      end

  end in
  print_endline "1";
  let cpp_model = create_MainModel model in
  print_endline "2";
  add_role cpp_model 555 "homm";
  print_endline "3";
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"myModel" cpp_model
;;
let () = Callback.register "doCaml" main
