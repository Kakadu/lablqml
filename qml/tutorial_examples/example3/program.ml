open QmlContext

let main () =
  let data =
    [ ("Анна Каренина",           "Лев Толстой")
    ; ("Война и Мир",             "Лев Толстой")
    ; ("Les Misérables",          "Victor Hugo")
    ; ("Les Trois Mousquetaires", "Alexandre Dumas, père")
    ] in

  let dataItems = List.map (fun (title,author) ->
    let cppobj = DataItem.create_DataItem () in
    object
      inherit DataItem.base_DataItem cppobj as super
      method author () = author
      method title () = title
    end
  ) data in

  let model_cppobj = MainModel.create_MainModel () in
  MainModel.add_role model_cppobj 555 "someRoleName";

  let model = object(self)
    inherit MainModel.base_MainModel model_cppobj as super
    method parent _ = QModelIndex.empty
    method columnCount _ = 1
    method index row column parent =
      if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
      else QModelIndex.empty
    method hasChildren _ = self#rowCount QModelIndex.empty > 0
    method rowCount _ = List.length dataItems
    method data index role =
      let r = QModelIndex.row index in
      if (r<0 || r>= List.length data) then QVariant.empty
      else begin
        match role with
          | 0 | 555 -> QVariant.of_object (List.nth dataItems r)#handler
          | _ -> QVariant.empty
      end
  end in
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"mainModel" model#handler

let () = Callback.register "doCamlInitialization" main
