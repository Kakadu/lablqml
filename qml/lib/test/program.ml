open QmlContext
external (|>) : 'a -> ('a -> 'b) -> 'b = "%revapply"

let main () =
  print_endline "Do something";
  let data = [["A"];["B";"C"]; ["D";"E";"F"]]
    |> List.map (List.map (fun str ->
      let cppobj = DataItem.create_DataItem () in
      object(self)
        inherit DataItem.base_DataItem cppobj as super
        method name () = str
        method sort () = String.lowercase str
      end
    ))
    |> List.map (fun (data: _ list) ->
      let cppobj = MiniModel.create_MiniModel () in
      MiniModel.add_role cppobj 555 "qwe";

      object(self)
        inherit MiniModel.base_MiniModel cppobj as super
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
            then QVariant.of_object (List.nth data r)#handler
            else QVariant.empty
          end
      end
    ) in

  let cpp_model = MainModel.create_MainModel () in
  MainModel.add_role cpp_model 555 "homm";

  let model = object(self)
    inherit MainModel.base_MainModel cpp_model as super
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
        then QVariant.of_object (List.nth data r)#handler
        else QVariant.empty
      end

  end in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"myModel" cpp_model
;;
let () = Callback.register "doCaml" main
