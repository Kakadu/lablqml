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
	  fprintf ch "    %s with out_name = %s, decalred in %s\n" 
	    (string_of_meth m) m.m_out_name m.m_declared
	);
	fprintf ch "  Pure virtuals\n";
	MethSet.iter set ~f:(fun (m,_) ->
	  fprintf ch "    %s with out_name = %s, declared in %s\n" 
	    (string_of_meth m) m.m_out_name m.m_declared
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

let overrides a b : bool = (MethSet.compare_items a b = 0) 
let (<=<) : MethSet.elt -> MethSet.elt -> bool = fun a b -> overrides a b 

let names_print_helper s ~set = 
  printf "%s: %s\n" s
    (MethSet.elements set
	|> List.to_string (fun (m,m') -> sprintf "(%s;%s)" m.m_out_name m'.m_out_name) ) 

(* Даны абстрактные методы в классе и нормальные методы.
   Выдрать какие норм. методы реализуют абстрактные, норм методы, которые ничего не оверрайдят
   и не реализованные абстрактные методы.
 *)
let super_filter_meths ~base ~cur = 
  let base_not_impl = ref MethSet.empty in
  let cur_impl = ref MethSet.empty in
  let cur_new = ref MethSet.empty in
  let rec loop base cur = 
    if MethSet.is_empty base then cur_new := MethSet.union !cur_new cur 
    else if MethSet.is_empty cur then base_not_impl := MethSet.union !base_not_impl base
    else begin
      let cur_el = MethSet.min_elt_exn cur  in
      let (a,b) = MethSet.partition ~f:(fun m -> cur_el <=< m) base in
      assert (MethSet.length a <= 1);
      if MethSet.is_empty a then begin
	(* метод cur_el не реализует ни одного абстрактного *)
	cur_new := MethSet.add !cur_impl cur_el;
	loop b (MethSet.remove cur cur_el)
      end else begin
	(* нашли абстрактный метод, реализованный в cur_el *)
	let el = MethSet.min_elt_exn a |> fst in
	cur_impl := MethSet.add_meth !cur_impl {fst cur_el with m_out_name = el.m_out_name }
      end
    end
  in
  loop base cur;
  (* ФИШКА: нельзя будет меня генерируемые имена у cur_impl, иначе не будет согласоваться 
     наследование с базовым (абстрактным) классом *)
  (!base_not_impl,!cur_impl,!cur_new)
    
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
    [(*"QIntegerForSize>"; "QWidget"; "QEvent"; "QStyleOption"; "QAccessible"; "QStyle"; "QLayout"; 
     "QGesture"; "QAccessible2Interface"; "QTextFormat"; "QAbstractState"; 
     "QAbstractAnimation"; "QPaintDevice"; "QAbstractItemModel"; 
     "QTextObject"; "QFactoryInterface";  "QtSharedPointer::ExternalRefCountWithDestroyFn" *)] in
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
      | None -> 
	evaluated := Evaluated.add !evaluated key;
	printf "base class of %s is not in index. skipped\n" (snd key) 
      | Some (Enum _) -> print_endline "In graph exists enum. nonsense"
      | Some (Class (c,_)) -> begin
	let base_keys = List.map c.c_inherits ~f:NameKey.key_of_fullname in
	printf "Base classes are: %s\n" (List.to_string snd base_keys);
	assert (List.for_all base_keys ~f:(Evaluated.mem !evaluated));
	evaluated := Evaluated.add !evaluated key;
	printf "Evaluating class %s\n" c.c_name;
	names_print_helper "Normal meths" c.c_meths_normal;
	names_print_helper "Abstract meths" c.c_meths_abstr;

	let bases_data = List.filter_map base_keys ~f:(fun key -> match SuperIndex.find !index key with
	  | Some (Class (y,set) ) -> 
(*	    printf "Some class found %s\n" y.c_name; *)
	    Some (y,set)
	  | None -> 
	    printf "WARNING! Base class %s of %s is not in index. Suppose that it is not abstract\n" 
	      (snd key) c.c_name;
	    None
	  | Some (Enum _) -> print_endline "nonsense"; assert false
	) in
(*	printf "Bases_data length = %d\n" (List.length bases_data); *)

	let module S = String.Set in
	let cache = ref S.empty in
	let put_cache s = cache := S.add !cache s in
	let rec make_name s = 
	  if S.mem !cache s then make_name (s^"_") else s in

	(* Now generate viruals for index *)
	let base_virtuals = List.map bases_data ~f:snd |> MethSet.union_list in
	let base_normals  = List.map bases_data ~f:(fun (c,_) -> c.c_meths_normal) in
	let base_normals = MethSet.union_list base_normals in

	names_print_helper ~set:base_normals "base_normals";
	names_print_helper ~set:base_virtuals "base_virtuals";
	let () = 
	  let f = fun (m,_) -> 
	    if (S.mem !cache m.m_out_name) then begin
	      printf "Cache is: %s\n" (S.elements !cache |> String.concat ~sep:",");
	      print_endline m.m_out_name;
	      assert false
	    end;
	    put_cache m.m_out_name
	  in
	  print_endline "iterating base_normals";
	  MethSet.iter base_normals ~f;
	  MethSet.iter base_virtuals ~f
	in

	let fix_names set = MethSet.map set ~f:(fun (m,m') ->
	  if S.mem !cache m.m_out_name then begin
	    let new_name = make_name m.m_out_name in
	    put_cache new_name;
	    ({m with m_out_name=new_name},{m' with m_out_name=new_name})
	  end else begin 
	    put_cache m.m_out_name;
	    (m,m')
	  end
	) in

	let (not_impl,cur_impl,cur_new) = super_filter_meths ~base:base_virtuals ~cur:c.c_meths_normal in
	names_print_helper ~set:not_impl "not_impl";
	names_print_helper ~set:cur_impl "cur_impl";
	names_print_helper ~set:cur_new  "cur_new";

	let my_abstrs = fix_names c.c_meths_abstr in
	let cur_new' = fix_names cur_new in
	let total_abstrs = MethSet.union not_impl my_abstrs in
	names_print_helper ~set:cur_new' "cur_new'";
	names_print_helper ~set:my_abstrs "my_abstrs";
	names_print_helper ~set:total_abstrs "total_abstrs";

	let new_normals =  MethSet.union_list [cur_impl; cur_new'; base_normals] in
	let ans_class = { {c with c_meths_abstr = total_abstrs } 
			     with c_meths_normal = new_normals } in

	let data = Class (ans_class, total_abstrs) in
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
