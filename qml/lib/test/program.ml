open Printf
open Helpers

let () = Printexc.record_backtrace true

let path = ref "/home/kakadu/.opam/4.00.1/lib/ocaml"
let () = Arg.parse
    [ ("-I", Arg.Set_string path, "Where to look for cmi files")
    ] (fun s -> printf "Unknown parameter %s\n" s; exit 0)
    "This is usage message"

open QmlContext

class virtual abstractListModel cppobj = object(self)
  inherit AbstractModel.base_AbstractModel cppobj as super
  method parent _ = QModelIndex.empty
  method index row column parent =
    if (row>=0 && row<self#rowCount parent) then QModelIndex.make ~row ~column:0
    else QModelIndex.empty
  method columnCount _ = 1
  method hasChildren _ = self#rowCount QModelIndex.empty > 0
end

let root = S.(build_tree (read_modules "/home/kakadu/.opam/4.00.1/lib/ocaml"))
let selected = ref [-1]
let cpp_data: (abstractListModel * DataItem.base_DataItem list) list ref  = ref []

let cpp_data_helper ys =
  let f xs =
    let data = List.map xs ~f:(fun {Tree.name;_} ->
      let cppObj = DataItem.create_DataItem () in
      object(self)
        inherit DataItem.base_DataItem cppObj as super
        method name () = name
        method sort () = "1"
      end) in
    (* creating miniModel for this list *)
    let cppobj = AbstractModel.create_AbstractModel () in
    AbstractModel.add_role cppobj 555 "qwe";

    let o =
      object(self)
        inherit abstractListModel cppobj as super
        method rowCount _ = List.length data
        method data index role =
          let r = QModelIndex.row index in
          if (r<0 || r>= List.length data) then QVariant.empty
          else begin
            if (role=0 || role=555) (* DisplayRole *)
            then QVariant.of_object (List.nth data ~n:r)#handler
            else QVariant.empty
          end
      end
    in
    (o,data)
  in
  List.map ys ~f

let initial_cpp_data () : (abstractListModel * DataItem.base_DataItem list) list =
  let xs = Tree.proj root [-1] in
  assert (List.length xs = 1);
  cpp_data_helper xs

let item_selected controller mainModel x y : unit =
  printf "Item selected: %d,%d\n%!" x y;
  let last_row = List.length !cpp_data - 1 in
  let (new_selected,redraw_from) = Tree.change_state !selected (x,y) root in
  let leaf_selected =
    assert (redraw_from <= List.length !cpp_data);
    (redraw_from=List.length new_selected)
  in
  selected := new_selected;
  (*printf "New selected: [%s]\n" (List.to_string !selected ~f:string_of_int);*)

  (*printf "Redraw from: %d, last_row=%d\n%!" redraw_from last_row;*)
  let cpp_data_head = List.take !cpp_data ~n:redraw_from in
  printf "1\n%!";
  if redraw_from <= last_row then begin
    printf "Delete some rows\n%!";
    mainModel#beginRemoveRows QModelIndex.empty redraw_from (List.length !cpp_data-1);
    cpp_data := cpp_data_head;
    mainModel#endRemoveRows ();
    printf "Rows deleted!\n%!"
  end else begin
    cpp_data := cpp_data_head;
  end;
  printf "1.1\n%!";
  let xs = Tree.proj root new_selected in
  assert (List.length xs = List.length new_selected);
  if leaf_selected then begin
    let cur_item = List.last xs |> List.nth ~n:(List.last new_selected) in
    let b = Buffer.create 500 in
    let fmt = Format.(formatter_of_buffer b) in
    printf "1.7\n%!";
    Printtyp.signature fmt [cur_item.Tree.internal];
    printf "2\n%!";
    Format.pp_print_flush fmt ();
    printf "Some leaf has been selected:\n%s\n%!" (Buffer.contents b);
    controller#updateDescription (Buffer.contents b);
  end else begin
    let xs = List.drop xs ~n:(List.length cpp_data_head) in
    let zs = cpp_data_helper xs in
    if List.length zs <> 0 then begin
      let from = List.length !cpp_data in
      let last = from + List.length zs-1 in
      printf "Adding rows from %d to %d\n%!" from last;
      mainModel#beginInsertRows QModelIndex.empty from last;
      cpp_data := !cpp_data @ zs;
      mainModel#endInsertRows ();
      printf "End inserting rows. cpp_data.length = %d\n%!" (List.length !cpp_data);
    end;
  end;(*
  print_endline "Compacting";
  Gc.compact ();*)
  printf "10000\n%!";
  assert (List.length !cpp_data = List.length new_selected)

let main () =
  cpp_data := initial_cpp_data ();

  let cpp_model = AbstractModel.create_AbstractModel () in
  AbstractModel.add_role cpp_model 555 "homm";

  let model = object(self)
    inherit abstractListModel cpp_model as super
    method rowCount _ =
      print_endline "???";
      List.length !cpp_data
    method data index role =
      let r = QModelIndex.row index in
      if (r<0 || r>= List.length !cpp_data) then QVariant.empty
      else begin
        if (role=0 || role=555) (* DisplayRole *)
        then QVariant.of_object (List.nth !cpp_data ~n:r |> fst)#handler
        else QVariant.empty
      end
  end in

  let controller_cppobj = Controller.create_Controller () in
  let controller = object(self)
    inherit Controller.base_Controller controller_cppobj as super
    method onItemSelected x y =
      printf "method onItemSelected %d %d\n%!" x y;
      try item_selected self model x y
      with exc ->
        Printexc.to_string exc |> print_endline;
        printf "Backtrace:\n%s\n%!" (Printexc.get_backtrace ());
        exit 0
    val mutable desc = None
    method isHasData () =
      match desc with Some _ -> true | _ -> false
    method getDescr () =
      print_endline "Inside method getDescr()";
      match desc with
        | Some x -> x
        | None   ->
            eprintf "App have tried to access description which should not exist now";
            "<no description. Bug!>"
    method updateDescription info =
      printf "inside updateDescription \"%s\"\n%!" info;
      if self#isHasData () then begin
        desc <- Some info;
      end else begin
        desc <- Some info;
        self#emit_hasDataChanged true;
      end;
      self#emit_descChanged info
  end in

  let () =
    let c = Gc.get () in
    c.Gc.max_overhead <- 1000001 (* disable compactions *)
  in

  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"myModel" model#handler;
  set_context_property ~ctx:(get_view_exn ~name:"rootContext") ~name:"controller" controller#handler



let () = Callback.register "doCaml" main
