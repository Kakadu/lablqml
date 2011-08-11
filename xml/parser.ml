open Core
module List = Core_list
module String = Core_string
module Set = Core_set
module Map = Core_map

open Simplexmlparser

let startswith ~prefix:p s = 
  if (String.length p > (String.length s)) then false
  else (Str.first_chars s (String.length p) = p)

let endswith ~postfix:p s = 
  if String.length p > (String.length s) then false
  else (Str.last_chars s (String.length p) = p)

type modifiers = Static | Abstract | Virtual

let (|>) a b = b a
let ($) a b = fun x -> a (b x)

type cpptype = { t_name:string; t_is_const:bool; t_indirections:int; t_is_ref:bool; t_params: cpptype list } 
and meth = cpptype * string * func_arg list (* void foo(int,int,int) *)
and func_arg = cpptype * string option (* type and default value *)
with sexp

let unreference = function
  | {t_name = t_name; t_indirections=t_indirections; t_is_const=t_is_const; t_params=t_params; _} ->
    let t_is_ref = false in
    { t_name; t_indirections; t_is_const; t_is_ref; t_params }

exception Ans of int
let wrap v = if v<>0 then raise(Ans v) 
let rec compare_cpptype a b = 
  try
    (compare a.t_is_const b.t_is_const) |> wrap;
    (compare a.t_indirections b.t_indirections) |> wrap;
    (compare a.t_is_ref b.t_is_ref) |> wrap;
    (compare a.t_name b.t_name) |> wrap;
    List.iter2 a.t_params b.t_params ~f:(fun a b -> compare_cpptype a b |> wrap);
    0
  with Ans x -> x
    
let compare_func_arg (t1,o1) (t2,o2) = 
  let c = compare_cpptype t1 t2 in
  if c<>0 then c else match (o1,o2) with
    | (Some a,Some b) -> compare a b
    | (Some _,None) -> 1
    | (None, Some _) -> -1
    | (None,None) -> 0
  
let compare_meth (res1,name1,l1) (res2,name2,l2) = 
  try
    compare name1 name2 |> wrap;
    compare (List.length l1) (List.length l2) |> wrap;
    compare_cpptype res1 res2 |> wrap;
    List.iter2 l1 l2 ~f:(fun a b -> compare a b |> wrap);
    0
  with Ans c -> c
    
module MethSet = Core_set.Make(struct
  type t = meth
  type sexpable = meth
  let sexp_of_t = sexp_of_meth
  let t_of_sexp = meth_of_sexp
  let compare = compare_meth
end)

type clas = { 
  c_inherits: string list;
  c_props: prop list;
  c_sigs: sgnl list;
  c_slots: slt list;
  c_meths_static: MethSet.t; (* public static *)
  c_meths_abstr:  MethSet.t; (* public pure virtual *)
  c_meths_normal: MethSet.t; 
  c_enums: enum list;
  c_constrs: constr list;
  c_name: string
}
and namespace = { ns_name:string; ns_classes:clas list; ns_enums:enum list; ns_ns: namespace list }
and enum = string * (string list)
and constr = func_arg list
and slt = string * (func_arg list)
and sgnl = string * (func_arg list)	
and prop = string * string option * string option	

let empty_namespace = { ns_name="empty"; ns_classes=[]; ns_enums=[]; ns_ns=[] }
(* convert class to pointer on this class *)
let typeP_of_class c = 
  { t_name=c.c_name; t_indirections=1; t_is_const=false; t_is_ref = false; t_params=[] }

(*
let meth2str (name,args,res,policy,modifiers) = 
  let isAbstract = List.mem Abstract modifiers in
  let isVirtual =  List.mem Virtual modifiers in
  let pol_str = match policy with 
    | Public -> "public" | Protected -> "protected" | Private -> "private" in
  let argsstr = Core_list.map args ~f:(fun (t,def) ->

  *)

let is_void_type t = (t.t_name = "void") && (t.t_indirections=0) 

let remove_defaults meth = match meth with
  | (res,name,lst) -> (res,name, List.map lst ~f:(fun (a,_) -> (a,None)))

let string_of_type t = 
  String.concat  	  
    [if t.t_is_const then "const " else "";
     t.t_name; String.make t.t_indirections '*'; if t.t_is_ref then " &" else ""]

let string_of_constr ~classname c = 
  let args_str = Core_list.map c ~f:(fun (t,def) ->
    (string_of_type t) ^ (match def with None -> "" | Some x -> " = " ^ x)) 
	   |> String.concat ~sep:", "
  in
  String.concat
    [classname;"("; args_str;" )"]

let string_of_meth (res,name,args) = 
  let args_str = Core_list.map args ~f:(fun (t,def) ->
    (string_of_type t) ^ (match def with None -> "" | Some x -> " = " ^ x)) 
	   |> String.concat ~sep:", "
  in
  (* additional space for OCaml compiler (comments) *)
  Printf.sprintf "%s %s(%s )"
    (string_of_type res) name args_str

let rec headl c lst =
  let rec helper c h tl = 
    if c=0 then (List.rev h,tl)
    else match tl with
      | hh::ll -> helper (c-1) (hh::h) ll
      | [] -> raise (Failure "headl")
  in
  helper c [] lst
(*
*)
(*
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
    *)

let skipNs = function
  | "std" | "QGL" | "internal" | "QtConcurrent" | "QtPrivate" | "QDrawBorderPixmap" 
  | "QtSharedPointer" | "QMdi" | "QAlgorithmsPrivate" | "QAccessible2" -> true
  | _ -> false

let fixTemplateClassName = 
  (Str.global_replace (Str.regexp "&lt;") "<") $
  (Str.global_replace (Str.regexp "&gt;") ">")

let str_replace ~patt init = List.fold_left 
  ~f:(fun aggr (patt, v) -> 
    Str.global_replace (Str.regexp patt) v aggr
  ) ~init patt



exception Break

let strip_dd ~prefix:p s =
  let plen = String.length p+2 in
  if (String.length s < plen) then s 
  else if (Str.first_chars s plen = (p^"::") ) then Str.string_after s plen 
  else s 

let attrib = List.assoc

let str2policy = function
  | "public" -> `Public
  | "protected" -> `Protected
  | "private" -> `Private
  | _ -> raise (Invalid_argument "str2policy") 

(********************* Parsing code *********************************)
let  parse_prop = function
  | ("property",("name",name)::_,lst) -> 
    let read = ref None in
    let wr = ref None in
    let foo = function
      | Element ("read",("value",v)::_,_) -> read := Some v
      | Element ("write",("value",v)::_,_) -> wr := Some v
      | _ -> () 
    in
    List.iter ~f:foo lst;
    (name,!read,!wr)
  | _ -> assert false
(****** Parsing argument ************************)
let rec parse_arg (_,attr,lst) = 
  let default = List.assoc_opt "default" attr in
  let t_indirections = List.assoc "indirections" attr |> int_of_string in
  let t_is_ref = List.assoc "isReference" attr |> bool_of_string in
  let t_is_const = List.assoc "isConstant" attr |> bool_of_string in
  let t_name = List.assoc "type" attr |> fixTemplateClassName in
  let t_params = 
    match lst with 
      | [Element ("arguments",_,lst)] ->
	let foo = function
	  | Element (("argument",_,_) as e) -> fst (parse_arg e)
	  | _ -> assert false
	in
	List.map ~f:foo lst
      | _ -> []
  in
  ({t_name; t_is_const; t_is_ref; t_indirections; t_params}, default)
  


let rec build root = match root with
  | PCData _ -> assert false
  | Element ("code",_,lst) -> 
    let classes = ref [] in
    let nss = ref [] in
    let enums = ref [] in

    List.iter lst ~f:(function
      | Element (("class",attr,lst) as e)    -> 
	classes := (parse_class "" e) :: !classes
      | Element (("namespace",_,lst) as e) -> 
	nss := (parse_ns "" e) :: !nss
      | Element ( ("enum",_,lst) as e) -> 
	enums := (parse_enum "" e) :: !enums
      | _ -> assert false);
(*    let classes = List.filter (fun c -> not (isTemplateClass c.c_name) ) !classes in *)
    {ns_name=""; ns_ns= !nss; ns_classes= !classes; ns_enums= !enums }
  | _ -> print_endline "XML file is incorrect";
    assert false

  
and parse_class nsname c  = 
  match c with
  | ("class",attr,lst) ->
    let name = attrib "name" attr in
    let name = strip_dd ~prefix:nsname name in
    let name = fixTemplateClassName name in
    
    let helper = 
      let aggr lst = 
	let args = ref [] in
	let ret = ref None in
	let modif  = ref `Normal in
	let policy = ref `Public in

	List.iter lst ~f:(function
	  | Element (("return",_,_) as e) -> 
	    ret := Some (parse_arg e |> fst)
	  | Element ("arguments",_,lst) ->
	    List.iter lst ~f:(function
	      | Element (("argument",_,_) as e) -> args := (parse_arg e) :: !args
	      | _ -> assert false)

	  | Element ("accessPolicy",("value",p)::_,_) -> policy := str2policy p
	  | Element ("modifiers",_,lst) -> 
	    List.map lst ~f:(function Element (n,_,_) -> n | PCData _ -> assert false) 
	    |> (fun set ->
		  if List.mem "abstract" ~set then modif := `Abstract 
		  else if List.mem "static" ~set then modif := `Static
		  else modif := `Normal)
	  |  _ -> assert false);
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

    List.iter lst ~f:(function
      | Element ("inherits",_,ll) ->
	List.iter ll ~f:(function 
	  | Element ("class",("name",nn)::_,_) -> inher := nn :: !inher
	  |  _ -> assert false) 
      | (Element ("function",_,ll)) as e -> 
	(let (name,args,ret,policy,modif) = helper e 
	 in 
	 match ret with
	   | Some r -> mems := (name,args,r,policy,modif) :: !mems
	   | None -> assert false)
      | (Element ("constructor",_,ll)) as e -> let (_,args,_,policy,modif) = helper e in
					       constrs := (args,policy) :: !constrs 	
      | (Element ("slot",_,ll)) as e -> let (a,b,_,_,_) = helper e in 
					slots := (a,b) :: !slots
      | (Element ("signal",_,ll)) as e -> let (a,b,_,_,_) = helper e in 
					  sigs := (a,b) :: !sigs
      | Element (("enum",_,_) as e) -> enums := (parse_enum name e) :: !enums
      | Element (("property",_,_) as e) -> props := (parse_prop e) :: !props
      | Element ("class",("name",nn)::_,_) ->
	let nn = fixTemplateClassName nn in
	print_endline ("skipping inner class " ^ name ^ "::" ^ nn)	
      | Element ("destructor",_,_) -> ()
      | _ -> assert false
    );
    let statics = ref MethSet.empty in
    let abstrs  = ref MethSet.empty in
    let normals = ref MethSet.empty in
    
    List.iter !mems ~f:(fun (mname,args,res,policy,modif) ->
      let m = (res,mname,args) in
      match (policy, modif) with
	| (`Private,`Abstract) -> 
	  raise (Common.Bug
		   (Printf.sprintf "Nonsense: private pure virtual function (class %s): %s" 
	  name (string_of_meth m)))
	| (`Protected,`Abstract) 	    
	| (`Public,`Abstract) -> 
	  (* removing defaults for normal find virtuals in graph *)
	  abstrs := MethSet.add !abstrs (remove_defaults m)
	| (`Private,`Static)
	| (`Protected, `Static) -> ()
	| (`Public, `Static) -> statics := MethSet.add !statics m
	| (`Private,`Normal)
	| (`Protected, `Normal) -> ()
	| (`Public, `Normal) -> normals := MethSet.add !normals m
    );
      
    let inherits = List.map ~f:fixTemplateClassName !inher in
    (* next remove base classes such as QSet<...>, QList<...>, QVector<...> 
       TODO: add special base classes.
       Also forget inheritance of template base classes
    *)
    let inherits = List.filter inherits ~f:(fun name ->
      if startswith ~prefix:"QList<" name then false 
      else if startswith ~prefix:"QVector<" name then false 
      else if startswith ~prefix:"QSet<" name then false 
      else match Core.Core_string.find name ~f:((=) '<') with
	| Some x -> false | None -> true) in
	
    let constrs = List.filter ~f:(fun (args,policy) -> match policy with
      | `Public -> true | `Private | `Protected -> false) !constrs in
    let constrs = List.map ~f:fst constrs in
    { c_name=name; c_constrs= constrs; c_slots= !slots; 
      c_meths_static= !statics; c_meths_abstr= !abstrs; c_meths_normal= !normals;
      c_inherits= inherits; c_enums= !enums; c_props= !props; c_sigs= !sigs }
  | _ -> assert false

and parse_enum superName = function
  | ("enum",("name",name)::_,lst) -> 
    let mems = ref [] in
    let foo = function
      | Element ("enumerator",_,[PCData name]) -> mems := name :: !mems
      | _ -> assert false
    in
    List.iter ~f:foo lst;
    (strip_dd ~prefix:superName name,!mems)
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
    List.iter ~f lst;
    {ns_name=name; ns_classes= !clas; ns_enums= !enums; ns_ns= !nss}
  | _ -> assert false

;;
