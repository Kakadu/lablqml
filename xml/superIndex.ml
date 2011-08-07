open Parser
open Core
module List = Core_list
module String = Core_string

module NameKey = struct
  type t = string list * string (* list of namespaces and classes (reversed) and concatenated string *)
  type sexpable = t
  let sexp_of_t t = Core_string.sexp_of_t (snd t)

  let key_of_fullname b = 
    let a =  Str.split (Str.regexp "::")  b |> List.rev in
    (a,b) 

  let t_of_sexp s = Core_string.t_of_sexp s |> key_of_fullname 

  let compare a b = Core_string.compare (snd a) (snd b) 

  let make_key ~name ~prefix = (* classname and reverted prefix *)
    let lst = name::prefix in
    (lst, String.concat ~sep:"::" (List.rev lst) )
  let hash (_,b) = String.hash b
  let equal (_,b) (_,d) = String.equal b d
end

type index_data = 
  | Class of clas * MethSet.t (* class data and its virtuals *)
  | Enum of enum

module SuperIndex = struct
  include Core_map.Make(NameKey)
end

type index_t = index_data SuperIndex.t

let is_enum_exn ~key t = match SuperIndex.find_exn t key with
  | Class _ -> false
  | Enum _ -> true

let is_class_exn ~key t = match SuperIndex.find_exn t key with
  | Class _ -> true
  | Enum _ -> false


module Evaluated = Core_set.Make(NameKey)
module V = NameKey

module G = struct 
  include Graph.Imperative.Digraph.Concrete(V) (* from base to subclasses *)
  let graph_attributes (_:t) = []
  let default_vertex_attributes _ = []
  let vertex_name v = snd (V.label v ) 
    |> Str.global_replace (Str.regexp "::") "_"
    |> Str.global_replace (Str.regexp "<") "_"
    |> Str.global_replace (Str.regexp ">") "_"
    |> Str.global_replace (Str.regexp ",") "_"
    |> Str.global_replace (Str.regexp "*") "_"
    
  let vertex_attributes v = [`Label (snd (V.label v)) ]
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []

  let rec kill_and_fall g root = 
    let lst = succ g root in
    remove_vertex g root;
    List.iter lst ~f:(kill_and_fall g)
end
module GraphPrinter = Graph.Graphviz.Dot(G)
    
let build_superindex root_ns = 
(* итак, нам поступают по очереди классы. У них есть базовые классы.
   строим граф где ребра идут от супеклассов к подклассам и запоминаем корни деревьев.
   при этом добавляем поступившие  классы в индекс.

   После этого определяем классы графа, которых нет в индексе

*)
  let index = ref SuperIndex.empty in
  let g = G.create () in

  let on_class_found prefix name c =
    let key = NameKey.make_key ~name ~prefix in
    let data = Class (c, MethSet.empty) in
    
    let curvertex = G.V.create key in
    G.add_vertex g curvertex;
    let bases = List.map c.c_inherits ~f:(fun basename ->
      let k = NameKey.key_of_fullname basename in
      G.V.create k ) in

    List.iter bases ~f:(fun from -> G.add_edge g from curvertex);
    index := SuperIndex.add ~key ~data !index
  and on_enum_found (prefix:string list) e = 
    let name = fst e in
    let key = NameKey.make_key ~name ~prefix in
    let data = Enum e in
    index:= SuperIndex.add ~key ~data !index
  in

  let rec iter_ns prefix ns = 
    let new_prefix = ns.ns_name :: prefix in
    List.iter ~f:(iter_ns new_prefix) ns.ns_ns;
    List.iter ~f:(iter_class new_prefix) ns.ns_classes;
    List.iter ~f:(on_enum_found new_prefix) ns.ns_enums;
    ()
  and iter_class prefix c = 
    on_class_found prefix c.c_name c;
    ()
  in

  print_endline "iterating root namespace";
  List.iter ~f:(iter_ns []) root_ns.ns_ns;
  List.iter ~f:(iter_class []) root_ns.ns_classes;
  List.iter ~f:(on_enum_found []) root_ns.ns_enums;

  (* simplify_graph *)
  let dead_roots = List.map ~f:NameKey.key_of_fullname 
    ["QIntegerForSize>"; (*"QEvent"; "QStyleOption";(* "QAccessible";*) "QStyle"; "QLayout"; 
(*     "QGesture"; "QAccessible2Interface"; "QTextFormat"; "QAbstractState"; 
     "QAbstractAnimation"; "QPaintDevice"; "QAbstractItemModel"; 
     "QTextObject"; "QFactoryInterface";*) *) "QtSharedPointer::ExternalRefCountWithDestroyFn"] in
  List.iter dead_roots ~f:(G.kill_and_fall g);

  let h = open_out "./outgraph.dot" in
  GraphPrinter.output_graph h g;
  close_out h;

  let get_vertexes_names lst = 
    List.map ~f:(fun v -> G.V.label v |> snd) lst |> List.fast_sort ~cmp:String.compare 
  in

  let roots = ref [] in
  G.iter_vertex (fun v -> if G.pred_e g v |> List.length = 0 then roots := v :: !roots) g; 
(*  let roots = ref [NameKey.key_of_fullname "QObject"] in *)
  let roots_names = get_vertexes_names !roots in
  print_endline "Root vertexes are:";
  List.iter roots_names ~f:(print_endline);

  (* Evaluating virtuals *)

  let evaluated = ref Evaluated.empty in  
  let cur_root_generation = ref (Evaluated.of_list !roots) in
  let some_changed = ref false in (* recursion exit condition *)

  let eval_class v = 
    let key = G.V.label v in
    match SuperIndex.find !index key with
      | None -> cur_root_generation := Evaluated.remove !cur_root_generation v;
	print_endline ("base class of " ^ (snd key) ^ "is not in index. skipped")
      | Some (Enum _) -> print_endline ("In graph exists enum. nonsense")
      | Some (Class (c,_)) ->
	Printf.printf "bases of `%s` are: %s\n" c.c_name (List.to_string (fun x->x) c.c_inherits);
	let base_keys = List.map c.c_inherits ~f:NameKey.key_of_fullname in
	let canEvaluate = List.for_all base_keys ~f:(Evaluated.mem !evaluated) in

	if canEvaluate then begin
	  print_endline ("evaluating class " ^ c.c_name );
	  assert (not (Evaluated.mem !evaluated key));
	  evaluated := Evaluated.add !evaluated key;
	  cur_root_generation := Evaluated.remove !cur_root_generation key;

	  (* now add subclasses to next root generation *)
	  let sons = G.succ g v in
	  List.iter sons ~f:(fun son -> cur_root_generation := Evaluated.add !cur_root_generation son);
	  some_changed := true;

	  (* Now add new viruals to index *)
	  let my_abstrs = c.c_meths_abstr in

	  let base_sets = List.map base_keys ~f:(fun key -> match SuperIndex.find !index key with
	    | None -> Printf.printf "Base class %s of %s is not yet indexed" (snd key) c.c_name;
	      assert false
	    | Some (Enum _) -> print_endline "nonsense"; assert false
	    | Some (Class (_,set)) -> set) in
	  let pre_ans = ref (MethSet.union_list (my_abstrs :: base_sets)) in

	  (* now remove implemented members *)
	  (* I think next line removes from 1st set all elements of second set *)
	  (* let ans = MethSet.diff pre_ans c.c_meths_normal in *)

	  MethSet.iter c.c_meths_normal ~f:(fun (res,name,lst) ->
	    let new_meth = (res,name,List.map lst ~f:(fun (a,_) -> (a,None))) in
	    pre_ans := MethSet.remove !pre_ans new_meth
	  );
	  let data = Class (c,!pre_ans) in
	  index := SuperIndex.add !index ~key ~data;
	end else begin
	  print_endline ("Can't evaluate clas " ^ c.c_name ^ " yet")
	end
  in
(*  let count = ref 0 in*)
  let rec loop () = 
(*    let () = (* print current root vertexes *)
      let names = get_vertexes_names (Evaluated.to_list !cur_root_generation) in
      "current roots (count = " ^ (string_of_int !count) ^ ")" |> print_endline;
      List.to_string (fun x->x) names |> print_endline;
      incr count
    in *)
    if Evaluated.is_empty !cur_root_generation then
      print_endline "Virtuals evaluation finished"
    else begin
      Evaluated.iter !cur_root_generation ~f:eval_class;
      if !some_changed then begin 
	loop ()
      end else begin
	print_endline "we have cycled in this classes";
	let names = get_vertexes_names (Evaluated.to_list !cur_root_generation) in
	List.to_string (fun x -> x) names |> print_endline;
	assert false
      end
    end

  in
  print_endline "Evaluating virtuals";
  loop ();

(*  print_endline "Now printing virtuals";
  SuperIndex.iter !index ~f:(fun ~key ~data -> begin
    match data with
      | Class (c,set) ->
	Printf.printf "class %s extends %s\n" c.c_name (List.to_string (fun x->x) c.c_inherits);
	MethSet.iter set ~f:(fun m -> Printf.printf "\t%s\n" (string_of_meth m) )
      | Enum (ename,lst) -> () (*print_endline ("enum " ^ ename)*)  
  end);  *)

(*  let testkey = NameKey.key_of_fullname "QGraphicsSimpleTextItem" in
  let () = match SuperIndex.find_exn !index testkey with
    | Class (c,_) ->
      Printf.printf "Members of class %s:\n" c.c_name;
      MethSet.iter c.c_meths_normal ~f:(fun m -> Parser.string_of_meth m |> print_endline)
      
    | Enum _ -> ()
  in *)
  (!index,g)
