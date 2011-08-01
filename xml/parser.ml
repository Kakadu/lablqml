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
and meth = string * func_arg list * (cpptype option) * accessPolicy * modifiers list
and constr = func_arg list * accessPolicy * modifiers list
and slt = string * (func_arg list)
and sgnl = string * (func_arg list)	
and prop = string * string option * string option	

exception Break

let startswith ~prefix:p s = 
  if (String.length p > (String.length s)) then false
  else (Str.first_chars s (String.length p) = p)

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

let str2type s = 
  let s = chop s in
  let isConst = startswith ~prefix:"const" s in
  let s = if isConst then Str.string_after s 5 else s in
  ()
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
    let nss : namespace list ref = ref [] in
    let enums: enum list ref = ref [] in

    List.iter (function
      | Element (("class",attr,lst) as e)    -> 
	classes := (parse_class "" e) :: !classes
      | Element (("namespace",_,lst) as e) -> 
	nss := (parse_ns "" e) :: !nss
      | Element ( ("enum",_,lst) as e) -> 
	enums := (parse_enum "" e) :: !enums
      | _ -> assert false) lst;
    {ns_name=""; ns_ns= !nss; ns_classes= !classes; ns_enums= !enums }
  | _ -> print_endline "XML file is incorrect";
    assert false
and parse_arg (_,attr,lst) : func_arg = 
  let default = attrib_opt "default" attr in
  let indir = attrib "indirections" attr |> int_of_string in
  let isRef = attrib "isReference" attr |> bool_of_string in
  let isConst = attrib "isConstant" attr |> bool_of_string in
  let typename = attrib "type" attr  in
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
    let name = Str.global_replace  (Str.regexp "&lt;") "<" name in
    let name = Str.global_replace  (Str.regexp "&gt;") ">" name in
    (* is generic class *)
(*    try let _ = String.index name '<' in ()
    with Not_found -> raise Break;
*)
    (* fix name *)    
    let name = strip_dd ~prefix:nsname name in
    
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
      | (Element ("function",_,ll)) as e -> let f = helper e in 
					    mems := f :: !mems
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

      | _ -> assert false
    in
    List.iter f lst;
    { c_name=name; c_constrs= !constrs; c_slots= !slots; c_meths= !mems; 
      c_inherits= !inher; c_enums= !enums; c_props= !props; c_sigs= !sigs }
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
    let name = strip_dd ~prefix:superName name in
    {ns_name=name; ns_classes= !clas; ns_enums= !enums; ns_ns= !nss}
  | _ -> assert false
