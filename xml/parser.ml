open Core
open Simplexmlparser

type accessPolicy = Public | Private | Protected
type modifiers = Static | Abstract | Virtual

let (|>) a b = b a
let ($) a b = fun x -> a (b x)
type clas = { c_inherits: string list;
	      c_props: prop list;
	      c_sigs: sgnl list;
	      c_slots: slt list;
	      c_meths: meth list;
	      c_enums: enum list;
	      c_constrs: constr list;
	      c_name: string
	    }
and namespace = { ns_name:string; ns_classes:clas list; ns_enums:enum list; ns_ns: namespace list }
and enum = string * (string list)
and cpptype = { t_name:string; t_is_const:bool; t_indirections:int; t_is_ref:bool; t_params: cpptype list }
and func_arg = cpptype * string option 
and meth = string * func_arg list * cpptype * accessPolicy * modifiers list
and constr = func_arg list * accessPolicy * modifiers list
and slt = string * (func_arg list)
and sgnl = string * (func_arg list)	
and prop = string * string option * string option	

let type2str t = 
  String.concat "" 	  
    [if t.t_is_const then "const " else "";
     t.t_name; String.make t.t_indirections '*';" "; if t.t_is_ref then "&" else ""]

let meth2str (name,args,res,policy,modifiers) = 
  let isAbstract = List.mem Abstract modifiers in
  let isVirtual =  List.mem Virtual modifiers in
  let pol_str = match policy with 
    | Public -> "public" | Protected -> "protected" | Private -> "private" in
  let argsstr = Core_list.map args ~f:(fun (t,def) ->
    (type2str t) ^ (match def with None -> "" | Some x -> " = " ^ x)) in
  Printf.sprintf "%s %s %s(%s)%s"
    pol_str (if isVirtual then "virtual" else "") name (String.concat "," argsstr)
    (if isAbstract then " = 0" else "")

let isAbstractMeth (_,_,_,_,mod') = 
  List.mem Abstract mod'

let rec headl c lst =
  let rec helper c h tl = 
    if c=0 then (List.rev h,tl)
    else match tl with
      | hh::ll -> helper (c-1) (hh::h) ll
      | [] -> raise (Failure "headl")
  in
  helper c [] lst

let unreference = function
  | {t_name = t_name; t_indirections=t_indirections; t_is_const=t_is_const; t_params=t_params; _} ->
    let t_is_ref = false in
    { t_name; t_indirections; t_is_const; t_is_ref; t_params }

let startswith ~prefix:p s = 
  if (String.length p > (String.length s)) then false
  else (Str.first_chars s (String.length p) = p)

let endswith ~postfix:p s = 
  if String.length p > (String.length s) then false
  else (Str.last_chars s (String.length p) = p)

let skipClass  = function
  | "Exception" -> true (* because it extends std::exception and I cant understand what to do *)
  | s when startswith ~prefix:"ExternalRefCount" s -> true
  | s when startswith ~prefix:"FilterKernel" s -> true
  | s when startswith ~prefix:"FilteredEach" s -> true
  | s when startswith ~prefix:"FilteredReducedKernel" s -> true
  | s when startswith ~prefix:"QList" s -> true
  | s when endswith ~postfix:"Interface" s -> true
  | s when startswith ~prefix:"QAccessible" s -> true
  | "ExternalRefCount<T>" -> true
  | _ -> false 


let skipNs = function
  | "std" | "QGL" | "internal" | "QtConcurrent" | "QtPrivate" | "QDrawBorderPixmap" 
  | "QtSharedPointer" | "QMdi" | "QAlgorithmsPrivate" | "QAccessible2" -> true
  | _ -> false

let isTemplateClass name = 
    try ignore (String.index name '<'); true
    with Not_found -> false

let isInnerClass name = 
  try let _ = Str.search_forward (Str.regexp "::") name 0 in true
  with Not_found -> false

let fixTemplateClassName = 
  (Str.global_replace (Str.regexp "&lt;") "<") $
  (Str.global_replace (Str.regexp "&gt;") ">")

let str_replace ~patt s = List.fold_left 
  (fun aggr (patt, v) -> 
    Str.global_replace (Str.regexp patt) v aggr
  ) s patt


let skipMeth ~classname name = 
  if startswith ~prefix:"operator" name then true
  else if startswith ~prefix:"d_func" name then true
  else match name with
    | "flush" 	
    | "initialize" (* QAccessible *) -> true (* because preprocesser has its own `flush` *)
    | "data" when classname = "QByteArray" ->  true
    | "bits" when classname = "QImage" ->  true
    | "scanLine" when classname = "QImage" ->  true
    | "invertedAppearance" when classname = "QProgressBar" ->  true
    | "cap" when classname = "QRegExp" ->  true
    | "pos" when classname = "QRegExp" ->  true
    | "errorString" when classname = "QRegExp" ->  true
    | "data" when classname = "QSharedMemory" ->  true
    | "shortcutId" when classname = "QShortcutEvent" ->  true
    | "isAmbiguous" when classname = "QShortcutEvent" ->  true
    | "toUnicode" when classname = "QTextCodec" ->  true
    | "indexOfTopLevelItem" when classname = "QTreeWidget" ->  true
    | "data" when classname = "QVariant" ->  true
    | "canConvert" when classname = "QVariant" ->  true
    | "toGraphicsObject" when classname = "QGraphicsItem" -> true (* very strange override *)     
    | _ when classname = "QMapData" -> true (* TODO: undestand this class. maybe dont generate it *)
    | "QThreadStorageData::QThreadStorageData" -> true
    | "QThreadStorageData" -> true (* cause it has a function-pointer parameter *)
    | _ -> false

exception Break

let strip_dd ~prefix:p s =
  let plen = String.length p+2 in
  if (String.length s < plen) then s 
  else if (Str.first_chars s plen = (p^"::") ) then Str.string_after s plen 
  else s 

(*
let chop s = 
  try 
    let loop init cond incr =
      let rec helper i = 
	if not (cond i) then None
	else if s.[i] <> ' ' then Some i
	else helper (incr i)
      in
      helper init
    in
    let len = String.length s in
    let l = loop 0 (fun i -> i<len) (fun i -> i+1) 
    and r = loop (len-1) (fun i -> i>=0) (fun i -> i-1) in

    match (l,r) with
      | Some l, Some r -> String.sub s l (r-l)
      | _ -> s
  with Not_found -> s

  *)  


let attrib = List.assoc
let attrib_opt k lst = try
		   Some (List.assoc k lst)
  with Not_found -> None

let xname = function
  | Element (n,_,_) -> n
  | PCData _ -> assert false
let attrib  = List.assoc
  
let rec build root = match root with
  | PCData _ -> assert false
  | Element ("code",_,lst) -> 
    let classes = ref [] in
    let nss = ref [] in
    let enums = ref [] in

    List.iter (function
      | Element (("class",attr,lst) as e)    -> 
	classes := (parse_class "" e) :: !classes
      | Element (("namespace",_,lst) as e) -> 
	nss := (parse_ns "" e) :: !nss
      | Element ( ("enum",_,lst) as e) -> 
	enums := (parse_enum "" e) :: !enums
      | _ -> assert false) lst;
(*    let nss = List.filter (fun ns -> not (skipNs ns.ns_name)) !nss in *)
    let classes = List.filter (fun c -> not (isTemplateClass c.c_name) ) !classes in
    {ns_name=""; ns_ns= !nss; ns_classes= classes; ns_enums= !enums }
  | _ -> print_endline "XML file is incorrect";
    assert false

and parse_arg (_,attr,lst) : func_arg = 
  let default = attrib_opt "default" attr in
  let indir = attrib "indirections" attr |> int_of_string in
  let isRef = attrib "isReference" attr |> bool_of_string in
  let isConst = attrib "isConstant" attr |> bool_of_string in
  let typename = attrib "type" attr |> fixTemplateClassName in
  let params : cpptype list = 
    match lst with 
      | [Element ("arguments",_,lst)] ->
	let foo = function
	  | Element (("argument",_,_) as e) -> fst (parse_arg e)
	  | _ -> assert false
	in
	List.map foo lst
      | _ -> []
  in
  ({t_name=typename; t_is_const=isConst; t_is_ref=isRef; t_indirections=indir; t_params=params}, default)
  
and parse_class nsname c  = 
  match c with
  | ("class",attr,lst) ->
    let name = attrib "name" attr in
    let name = strip_dd ~prefix:nsname name in
    let name = fixTemplateClassName name in

    (* is generic class *)
(*    try let _ = String.index name '<' in ()
    with Not_found -> raise Break;
*)
    (* fix name *)    
    
    let helper = 
      let aggr lst = 
	let args = ref [] in
	let ret = ref None in
	let modif  = ref [] in
	let policy = ref Public in
	let str2policy = function
	  | "public" -> Public
	  | "protected" -> Protected
	  | "private" -> Private
	  | _ -> assert false in

	List.iter (function
	  | Element (("return",_,_) as e) -> 
	    ret := Some (parse_arg e |> fst)
	  | Element ("arguments",_,lst) ->
	    lst |> List.iter (function
	      | Element (("argument",_,_) as e) -> args := (parse_arg e) :: !args
	      | _ -> assert false)

	  | Element ("accessPolicy",("value",p)::_,_) -> policy := str2policy p
	  | Element ("modifiers",_,lst) -> 
	    lst |> List.map (function Element (n,_,_) -> n | PCData _ -> assert false)
	        |> List.iter (function
		    | "static" -> modif := Static :: !modif 
		    | "virtual" -> modif := Virtual :: !modif 
		    | "abstract" -> modif := Abstract :: !modif
		    | _ -> assert false)
	  |  _ -> assert false) lst;
	(List.rev !args, !ret, !policy, !modif)
      in
      function
	| Element (_,("name",name)::_,lst) -> let (a,b,c,d) = aggr lst in (name,a,b,c,d)
	| _ -> assert false
    in

    let props = ref [] in
    let mems = ref [] in
    let sigs = ref [] in
    let slots = ref [] in
    let inher = ref [] in
    let enums = ref [] in
    let constrs = ref [] in 

    let f = function
      | Element ("inherits",_,ll) ->
	List.iter (function 
	  | Element ("class",("name",nn)::_,_) -> inher := nn :: !inher
	  |  _ -> assert false) ll
      | (Element ("function",_,ll)) as e -> 
	(let (name,args, ret,policy,modif) = helper e in 
	 match ret with
	   | Some r -> mems := (name,args,r,policy,modif) :: !mems
	   | None -> assert false)
      | (Element ("constructor",_,ll)) as e -> let (_,args,_,policy,modif) = helper e in
					       constrs := (args,policy,modif) :: !constrs 
	
      | (Element ("slot",_,ll)) as e -> let (a,b,_,_,_) = helper e in 
					slots := (a,b) :: !slots
      | (Element ("signal",_,ll)) as e -> let (a,b,_,_,_) = helper e in 
					  sigs := (a,b) :: !sigs
      | Element (("enum",_,_) as e) -> enums := (parse_enum name e) :: !enums
      | Element (("property",_,_) as e) -> props := (parse_prop e) :: !props
      | Element ("class",("name",nn)::_,_) ->
	print_endline ("skipping inner class " ^ name ^ "::" ^ nn)	
      | Element ("destructor",_,_) -> ()
      | _ -> assert false
    in
    List.iter f lst;
    let mems = List.filter (fun (mname,_,_,_,_) -> not (skipMeth ~classname:name mname) ) !mems in
    let inherits = List.map fixTemplateClassName !inher in
    (* next remove base classes such as QSet<...>, QList<...>, QVector<...> 
       TODO: add special base classes.
       Also forget inheritance of template base classes
    *)
    let inherits = inherits |> List.filter (fun name ->
      if startswith ~prefix:"QList<" name then false 
      else if startswith ~prefix:"QVector<" name then false 
      else if startswith ~prefix:"QSet<" name then false 
      else match Core.Core_string.find name ~f:((=) '<') with
	| Some x -> false | None -> true) in
	
    { c_name=name; c_constrs= !constrs; c_slots= !slots; c_meths= mems; 
      c_inherits= inherits; c_enums= !enums; c_props= !props; c_sigs= !sigs }
  | _ -> assert false

and parse_enum superName c: enum = match c with
  | ("enum",("name",name)::_,lst) -> 
    let mems = ref [] in
    let foo = function
      | Element ("enumerator",_,[PCData name]) -> mems := name :: !mems
      | _ -> assert false
    in
    List.iter foo lst;
    (strip_dd ~prefix:superName name,!mems)
  | _ -> assert false

and parse_prop = function
  | ("property",("name",name)::_,lst) ->
    let read = ref None in
    let wr = ref None in
    let foo = function
      | Element ("read",("value",v)::_,_) -> read := Some v
      | Element ("write",("value",v)::_,_) -> wr := Some v
      | _ -> () 
    in
    List.iter foo lst;
    (name,!read,!wr)
  | _ -> assert false
    
and parse_ns superName = function
  | ("namespace",("name",name)::_,lst) -> 
    let name = strip_dd ~prefix:superName name in
    let clas = ref [] in
    let enums = ref [] in
    let nss = ref [] in 
    let f = function
      | Element (("namespace",_,_) as e) ->
	nss := (parse_ns name e) :: !nss
      | Element (("class",_,_)  as e) ->
	clas := (parse_class name e) :: !clas
      | Element ( ("enum",_,_) as e) ->
	enums := (parse_enum name e) :: !enums
      | _ -> assert false
    in
    List.iter f lst;
    {ns_name=name; ns_classes= !clas; ns_enums= !enums; ns_ns= !nss}
  | _ -> assert false

(* name index *)
type short_method = string * cpptype list
type indexItem = | Ns of namespace 
		 | Class of clas * short_method list (* clas and its pure virtual members *)
		 | Enum of enum

exception CompareAns of int
let mycomparer (n1,lst1) (n2,lst2) =
  try
    let nameres = String.compare n1 n2 in
    if nameres <> 0 then 
      raise (CompareAns nameres); 
    let len1 = List.length lst1 and len2 = List.length lst2 in
    if len1<>len2 then 
      raise (CompareAns (compare len1 len2));
    Core_list.iter2 lst1 lst2 ~f:(fun t1 t2 ->
      let nameres = compare t1.t_name t2.t_name in
      if nameres<>0 then raise (CompareAns nameres);
      let indres = compare t1.t_indirections t2.t_indirections in
      if indres <> 0 then raise (CompareAns indres);
      if t1.t_is_ref <> t2.t_is_ref then 
	raise (CompareAns (if t1.t_is_ref then -1 else 1));
      if t1.t_is_const <> t2.t_is_const then 
	raise (CompareAns (if t1.t_is_const then -1 else 1))
    );
    0
  with CompareAns ans -> ans      
      
exception NotInIndex of string
module Index = struct 
  include Core_map.Make(Core_string)
  
  let print_keys t = iter ~f:(fun ~key ~data -> print_endline key) t
  let isEnum name t = 
    try
      match find_exn t name with
	| Enum _ -> true
	| _ -> false
    with Not_found -> raise (NotInIndex name)
  let isClass name t = 
    try
      match find_exn t name  with
	| Class _ -> true | _ -> false
    with Not_found -> raise (NotInIndex name)

  let find_class_exn t name =
      match find_exn t name with 
	| Class (c,lst) -> c,lst
	| _ -> raise Not_found

end
type index_t = indexItem Index.t

let build_index nslst = 
  let m = ref Index.empty in
  let add k v = 
    if Index.mem !m k  then 
      print_endline ("Adding dublicate with name " ^ k);
      
    m:= Index.add k v !m
  in
  let rec iter_ns ns = 
    add ns.ns_name (Ns ns);
    List.iter iter_ns ns.ns_ns;
    List.iter iter_class ns.ns_classes;
    List.iter (iter_enum (Some ns.ns_name) ) ns.ns_enums;
    ()
  and iter_class c = 
    if not (skipClass c.c_name) then (
      add c.c_name (Class (c,[]) );
      List.iter (iter_enum (Some c.c_name)) c.c_enums
    )
  and iter_enum parent_name e = 
    let k = (match parent_name with None -> "" | Some x -> x^"::") ^ (fst e) in
    add k (Enum e)
  in
  List.iter iter_ns nslst;
  !m

(* check if class c is in index then all base classes of c are in index to *)
let checkBasesExists index = 
  try
    Index.iter (fun ~key ~data -> match data with
      | Ns ns -> ()
      | Class (c,_) -> 
	Core_list.iter ~f:(fun name -> if not (Index.mem index name) then raise Break) c.c_inherits
      | Enum e -> ()) index;
    true
  with Break -> false  
(*
let removeClassesWithoutBases index = 
  let cur_index = ref index in
  let removed = ref false in
  let rec loop () = 
    removed:= false;
    let index = Index.iter !cur_index ~f:(fun ~key ~data -> match data with
      | Ns _ | Enum _ -> ()
      | Class (c,_) -> 
	let inher = c.c_inherits in
	if not (Core_list.for_all inher ~f:(fun name -> Index.mem !cur_index name)) then (
	  print_endline ("Remove class from index: " ^ key);
	  cur_index := Index.remove !cur_index key; 
	  removed:= true 
	)
    ) in
    if !removed then ( loop ())
    else ()
  in
  !cur_index
  *)
module StringSet = Core_set.Make(Core_string);; 
(*module to memoize aggregated item. Good to use Hashtbl module but I can't guess hash function for string *)
(*
let postBuildIndex index = 
  (* assert: no cycles *)
  let was = ref StringSet.empty in
  let keys = Index.keys index in
  let index = ref index in

  let rec doClass c =
    let name = c.c_name in
    print_endline ("doClass " ^ name);
    let inher = c.c_inherits in
    let virtuals: short_method list = Core_list.map inher ~f:(fun basename ->
      print_endline ("    basename: " ^ basename);
      if startswith ~prefix:"QList" basename then []
      else if startswith ~prefix:"QVector" basename then []
      else if StringSet.mem (!was) basename then begin
	match Index.find_exn !index basename with
	  | Class (_,lst) -> lst
	  | _ -> assert false
      end else begin
	doClass (Index.find_class_exn !index basename |> fst);
	match Index.find_class_exn !index basename with
	  | (_,lst) -> lst
	  | _ -> assert false
      end
    ) |> List.concat |> Core_list.dedup ~compare:mycomparer in

    (* Now remove virtuals implemented in class `name` *)
    let class_virtuals = ref [] 
    and class_normal = ref [] in

    Core_list.iter c.c_meths ~f:(fun (name,args,_,_,modifs) ->
      let member: short_method = (name, args |> Core_list.map ~f:fst) in
      if Core_list.mem ~set:modifs Abstract then 
	class_virtuals := member :: !class_virtuals 
      else 
	class_normal := member :: !class_normal         
    );
    let ans = (Core_extended.Extended_list.diff virtuals !class_normal) @ !class_virtuals in

    index := Index.add ~key:name ~data:(Class (c,ans)) !index;
    was := StringSet.add !was name
  in
  Core_list.iter keys ~f:(fun name ->
    let v = Index.find_exn !index name in
    match v with
      | Ns _ | Enum _ -> ()
      | Class (c,_) when StringSet.mem !was c.c_name -> ()
      | Class (c,_) ->  doClass c
  );
  !index
*)
