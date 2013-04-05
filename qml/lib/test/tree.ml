open Helpers
open Printf

type 'a tree = {
  name: string;
  internal: 'a;
  sons: 'a tree list;
}
open Types

let name_of_item = function
  | Types.Sig_value     ({Ident.name;_},_)   -> name
  | Types.Sig_type      ({Ident.name;_},_,_) -> name
  | Types.Sig_exception ({Ident.name;_},_)   -> name
  | Types.Sig_module    ({Ident.name;_},_,_) -> name
  | Types.Sig_modtype   ({Ident.name;_},_)   -> name
  | Types.Sig_class     ({Ident.name;_},_,_) -> name
  | Types.Sig_class_type({Ident.name;_},_,_) -> name

let rec of_sig_item internal =
  match internal with
  | Types.Sig_value     ({Ident.name;_},_)  -> {name; internal; sons=[]}
  | Types.Sig_type      ({Ident.name;_},_,_)  -> {name; internal; sons=[]}
  | Types.Sig_exception ({Ident.name;_},_)  -> {name; internal; sons=[]}
  | Types.Sig_module    ({Ident.name;_},Types.Mty_signature sons,_) ->
      {name; internal; sons=List.map sons ~f:of_sig_item}
  | Types.Sig_module    ({Ident.name;_},_,_) -> {name; internal; sons=[]}
  | Types.Sig_modtype   ({Ident.name;_},_)   -> {name; internal; sons=[]}
  | Types.Sig_class     ({Ident.name;_},_,_) -> {name; internal; sons=[]}
  | Types.Sig_class_type({Ident.name;_},_,_) -> {name; internal; sons=[]}

let print_sig fmt v =
  match v with
  | Types.Sig_value     (id,desc)    -> Printtyp.value_description id fmt desc
  | Types.Sig_type      (id,desc,_)  -> Printtyp.type_declaration  id fmt desc
  | Types.Sig_exception (id,desc)    -> Printtyp.exception_declaration  id fmt desc
  | Types.Sig_module    (id,desc,_)  -> Format.fprintf fmt "<no data>\n"
  | Types.Sig_modtype   (id,desc)    -> Printtyp.modtype_declaration  id fmt desc
  | Types.Sig_class     (id,desc,_) -> Printtyp.class_declaration   id fmt desc
  | Types.Sig_class_type(id,desc,_) -> Printtyp.cltype_declaration  id fmt desc;
  Format.pp_print_newline fmt ();
  Format.pp_print_flush fmt ()

(* [old_selected] previous indexes. Either all>=0 or (last=-1 and others >=0) *)
let change_state (old_selected: int list) (x,y) root =
  assert (x>=0); (*
  printf "Change state\n";
  printf "Old_selected: %s\n" (List.to_string old_selected ~f:string_of_int); *)
  let selected_prefix = List.take old_selected ~n:x in
  let selected_tree =
    List.fold_left (selected_prefix) ~init: root ~f:(fun root n ->
      assert (List.length root.sons>n);
      List.nth root.sons ~n
    ) in
(*  let () = if (x=0) then assert (selected_tree = root) in*)
  assert (List.length selected_tree.sons > y);
  let new_selected = selected_prefix@[y] in (*
  printf "New selected: %s\n" (List.to_string new_selected ~f:string_of_int);*)
  let selected_item = List.nth selected_tree.sons ~n:y in
  let new_selected =
    if List.length selected_item.sons > 0 then new_selected@[-1]
    else new_selected
  in (*
  printf "New selected: %s\n" (List.to_string new_selected ~f:string_of_int);*)

  (* We should decide which minimal index to redraw *)
  let redraw_start_from = (x+1) in
  (new_selected, redraw_start_from)

let good_selected selected =
  List.for_all selected ~f:((<=)0) || begin
    let xs = List.rev selected in
    let (h,tl) = List.(hd xs, tl xs) in
    (h= -1) && (List.for_all tl ~f:((<=)0) )
  end

let print_tree root f selected =
  assert (good_selected selected);
  let x = ref root in
  List.iter selected ~f:(fun n ->
    (* print x.sons and put x[n] to x *)
    let stars = List.make (List.length !x.sons - 1) ~f:(fun i -> if i=n then "*" else "") in
    (*printf "Stars.length = %d, sons.length = %d\n" (List.length stars) (List.length !x.sons);*)
    printf "[%s]\n"
      (List.map2 stars !x.sons ~f:(fun s {name;_} -> sprintf "%s%s" s name)
          |> String.concat ",");
    if n= -1
    then printf "End.\n%!"
    else x:= List.nth !x.sons ~n
  )

let proj root (selected: int list) : _ tree list list =
  assert (good_selected selected);
  let ans = ref [root.sons] in
  List.iteri selected ~f:(fun i n ->
    if n = -1 then begin
      assert (i+1 = List.length selected)
    end else begin
      let x = List.hd !ans in
      let y = List.nth ~n x in
      if y.sons=[] then begin
        assert (i+1 = List.length selected)
      end else
        ans:= y.sons :: !ans
    end
  );
  List.rev !ans

let string_of_proj xs = List.to_string ~f:string_of_int xs
