open Parser
open Core
open Printf
module List = Core_list
module String = Core_string

module NameKey = struct
  type t = string list * string (* list of namespaces and classes (reversed) and concatenated string *)
  type sexpable = t
  let sexp_of_t t = Core_string.sexp_of_t (snd t)

  let key_of_fullname b = 
    let a =  Str.split (Str.regexp "::")  b |> List.rev in
    (a,b) 

  let key_of_prefix lst = match lst with
    | [] -> raise (Invalid_argument "NameKey.key_of_prefix: []")
    | lst -> (lst, List.rev lst |> String.concat ~sep:"::")
      
  let t_of_sexp s = Core_string.t_of_sexp s |> key_of_fullname 

  let compare a b = Core_string.compare (snd a) (snd b) 

  let make_key ~name ~prefix = (* classname and reverted prefix *)
    let lst = name::prefix in
    (lst, String.concat ~sep:"::" (List.rev lst) )
  let hash (_,b) = String.hash b
  let equal (_,b) (_,d) = String.equal b d
  let to_string (lst,_) = String.concat ~sep:"::" (List.rev lst)
end

type index_data = 
  | Class of clas * MethSet.t (* class data with all virtuals *)
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

  let to_channel t ch =
    SuperIndex.iter t ~f:(fun ~key ~data -> match data with
      | Enum e -> fprintf ch "Enum %s\n" (fst e)
      | Class (c,set) -> begin
	fprintf ch "Class %s\n" (NameKey.to_string key);
	fprintf ch "  Normal meths\n";
	MethSet.iter c.c_meths_normal ~f:(fun (m,_) ->
	  fprintf ch "    %s with out_name=%s\n" (string_of_meth m) m.m_out_name 
	);
	fprintf ch "  Pure virtuals\n";
	MethSet.iter set ~f:(fun (m,_) ->
	  fprintf ch "    %s with out_name=%s\n" (string_of_meth m) m.m_out_name
	);
	fprintf ch "************************************************** \n"
      end
    )

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
(*
(* change m.m_name in case of overrides. m_old_cpp_name will contain old name *)
let fix_meth_names set = 
  let module S=String.Set in
  let cache = ref S.empty in
  let rec make_name s = 
    if S.mem !cache s then make_name (s^"_") else s
  in
  MethSet.iter set ~f:(fun (m1,m2) -> 
    let new_name = make_name m1.m_name in
    m1.m_name <- new_name;
    m2.m_name <- new_name
  )
  *)

let overrides a b : bool = (MethSet.compare_items a b = 0) 
let (<=<) : MethSet.elt -> MethSet.elt -> bool = fun a b -> overrides a b 
    
let build_superindex root_ns = 
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
  and on_enum_found prefix e = 
    let name = fst e in
    let key = NameKey.make_key ~name ~prefix in
    let data = Enum e in
    index:= SuperIndex.add ~key ~data !index
  in

  let rec iter_ns prefix ns = 
    let new_prefix = ns.ns_name :: prefix in
    List.iter ~f:(iter_ns new_prefix) ns.ns_ns;
    List.iter ~f:(iter_class new_prefix) ns.ns_classes;
    List.iter ~f:(on_enum_found new_prefix) ns.ns_enums
  and iter_class prefix c = 
    on_class_found prefix c.c_name c
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

  let module Top = Graph.Topological.Make(G) in
  (* so we have classes topologically sorted. Base classes are before *)

  (* Evaluating virtuals *)
  let evaluated = ref Evaluated.empty in  
  let module Q = Core.Core_queue in (* keys queue *)
  let ans_queue = Ref.create (Q.create ()) in

  print_endline "Evaluating virtuals";
  g |> Top.iter (fun v ->
    let key = G.V.label v in
    match SuperIndex.find !index key with
      | None -> printf "base class of %s is not in index. skipped\n" (snd key) 
      | Some (Enum _) -> print_endline "In graph exists enum. nonsense"
      | Some (Class (c,_)) -> begin
	let base_keys = List.map c.c_inherits ~f:NameKey.key_of_fullname in
	assert (List.for_all base_keys ~f:(Evaluated.mem !evaluated));
	evaluated := Evaluated.add !evaluated key;

	let bases_data = List.map base_keys ~f:(fun key -> match SuperIndex.find !index key with
	  | Some (Class (y,set) ) -> (y,set)
	  | None -> printf "Base class %s of %s is not yet indexed\n" (snd key) c.c_name;
	    assert false
	  | Some (Enum _) -> print_endline "nonsense"; assert false
	) in
	(* Now generate viruals for index *)
	let base_virtuals = List.map bases_data ~f:(fun (_,set) -> set) in
	let module S = String.Set in
	let cache = ref S.empty in
	let put_cache s = cache := S.add !cache s in

	let evaluated_meths = ref MethSet.empty in
	let non_evaluated_meths = ref c.c_meths_normal in

	let base_virtuals = MethSet.filter (MethSet.union_list base_virtuals) ~f:(fun (m,m') ->
	  (* for each base virtual find normal method which overrides base 
	     in that case set take out_name of normal meth from base_meth *)
	  put_cache m.m_out_name;
	  let ans = ref true in 
	  let (a,b) = MethSet.partition !non_evaluated_meths ~f:(fun (mm,mm') ->
	    if (mm,mm') <=< (m,m') then begin
		(* we can evaluate this meth *)
	      ans:= false; 
	      mm.m_out_name <- String.copy m.m_out_name; 
	      mm'.m_out_name <- String.copy m.m_out_name;
	      if endswith ~postfix:"_" mm.m_out_name then
		assert (not (mm.m_name == m.m_out_name));
	      true
	    end 
	    else false
	  ) in
	    (* so we accumulated inherited members in `a` and own class's meths in b *)
	  evaluated_meths := MethSet.union !evaluated_meths a; 
	  non_evaluated_meths := b;
	  !ans
	) 
	in
	(* now in base_virtuals we have base's class meths that are not implemented in current class *)
	  
	let rec make_name s = 
	  if S.mem !cache s then make_name (s^"_") else s in

	MethSet.iter !non_evaluated_meths ~f:(fun (m,m') ->
	  let new_name = make_name m.m_out_name in
	  m.m_out_name <- new_name;
	  m'.m_out_name <- new_name;
	  assert (not (endswith ~postfix:"_" m.m_name))
	);
	let ans_normals = MethSet.union !non_evaluated_meths !evaluated_meths in

	let new_c = { c with c_meths_normal= ans_normals } in
	let data = Class (new_c, base_virtuals) in
	index := SuperIndex.add !index ~key ~data;
	Q.enqueue !ans_queue key
      end
  );
(*
  let get_vertexes_names lst = 
    List.map ~f:(fun v -> G.V.label v |> snd) lst |> List.stable_sort ~cmp:String.compare 
  in

  let roots = ref [] in
  G.iter_vertex (fun v -> if G.pred_e g v |> List.length = 0 then roots := v :: !roots) g; 
  let roots_names = get_vertexes_names !roots in
  print_endline "Root vertexes are:";
  List.iter roots_names ~f:(print_endline);
  *)
  print_endline "Now printing virtuals";
  SuperIndex.iter !index ~f:(fun ~key ~data -> begin
    match data with
      | Class (c,set) ->
	Printf.printf "class %s extends %s\n" c.c_name (List.to_string (fun x->x) c.c_inherits);
	MethSet.iter set ~f:(fun (m,_) -> Printf.printf "\t%s\n" (string_of_meth m) )
      | Enum (ename,lst) -> ()
  end);

  (!index,g, !ans_queue)
