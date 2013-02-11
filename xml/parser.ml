open Core
open Core.Std
open Printf
open Sexplib.Conv
open Simplexmlparser

let (%<) f g = fun x -> f (g x)
let (|>) a b = b a

(** receives pairs (enum_member,value) and returns list where map ~f:snd lst is disjunct 
 *  Needed to make compilable C++ switch statement when casting to Caml
 *  Idea is to filter enum members like
 *         member1 = member0, ...
 *)
let filter_enum_members lst = 
  let module S = String.Set in
  let value_set = ref S.empty in
  List.fold lst ~init:[] ~f:(fun acc (x,y) -> 
    value_set := S.add !value_set x;    
    match y with
      | None  -> acc
      | Some y when S.mem !value_set y -> acc 
      | Some y -> value_set := S.add !value_set y; x::acc )

let make_out_name ~classname cpp_name = match (classname,cpp_name) with
  | ("QGraphicsItem", "children") -> "children1"
  | (_,"done") -> "done1"
  | (_,"begin") -> "begin1"
  | (_,"end") -> "end1"
  | (_,"open") -> "open1"
  | (_,"type") -> "type1"
  | (_,"object") -> "object1"
  | (_,"match") -> "match1"
  | (_,"method") -> "method1"
  | _ -> cpp_name

let endswith ~postfix:p s = 
  if String.length p > (String.length s) then false
  else (Str.last_chars s (String.length p) = p)

let string_split ~on:d str = 
  let dlen = String.length d in
  let matches i = 
    try
      for j=0 to dlen-1 do
	if d.[j] <> str.[j+i] then raise Not_found
      done;
      true
    with Not_found -> false (* Reused existing exception *)
  in
  let rec loop curlen i ans = 
    assert (i>=curlen);
    if i >= String.length str then (
      if curlen>0 then (String.sub str ~pos:(i-curlen) ~len:curlen) :: ans else ans
    ) else if matches i then (
      let s = String.sub str ~pos:(i - curlen) ~len:curlen in
      loop 0 (i+dlen) (s::ans)
    ) else (
      loop (curlen+1) (i+1) ans
    )
  in
  loop 0 0 [] |> List.rev

type cpptype = { t_name:string; t_is_const:bool; t_indirections:int; t_is_ref:bool; t_params: cpptype list } 
and func_arg = { arg_type:cpptype; arg_name:string option; arg_default: string option }
and meth = { 
  m_res:cpptype;
  m_name:string;
  m_args:func_arg list;
  m_declared: string;
  m_out_name:string;
  m_access:[`Public | `Protected| `Private];
  m_modif: [`Normal | `Static   | `Abstract]
} 
with sexp 

let simple_arg arg_type = { arg_type; arg_name=None; arg_default = None }

let is_public = function `Public -> true | `Private -> false | `Protected -> false
let is_virtual = function `Virtual -> true | `Static | `Abstract -> false

let void_type = {t_name="void"; t_is_const=false; t_indirections=0; t_is_ref=false; t_params=[] }

let remove_defaults meth = match meth with
  | {m_res;m_name; m_args; m_declared; m_out_name; m_access; m_modif } -> 
    let m_args = List.map m_args ~f:(fun x -> {x with arg_default=None} ) in
    {m_res;m_name; m_declared; m_args; m_out_name; m_access; m_modif }

let unreference = function
  | {t_name=t_name; t_indirections=t_indirections; t_is_const=t_is_const; t_params=t_params; _} ->
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
    List.iter2_exn a.t_params b.t_params ~f:(fun a b -> compare_cpptype a b |> wrap);
    0
  with Ans x -> x
    (*
let compare_func_arg (t1,o1) (t2,o2) = 
  let c = compare_cpptype t1 t2 in
  if c<>0 then c else match (o1,o2) with
    | (Some a,Some b) -> compare a b
    | (Some _,None) -> 1
    | (None, Some _) -> -1
    | (None,None) -> 0
      *)
let compare_meth m1 m2 = 
  match (m1,m2) with
    | ({ m_name=name1; m_args=lst1;m_res=res1; _}, { m_name=name2; m_args=lst2;m_res=res2; _}) ->      
      try
	compare name1 name2 |> wrap;
	compare (List.length lst1) (List.length lst2) |> wrap;
	compare_cpptype res1 res2 |> wrap;
	List.iter2_exn lst1 lst2 ~f:(fun a b -> compare a b |> wrap);
	(* I know that I dont compare declaring class's names*)
	0
      with Ans c -> c
    
module MethKey = struct
  type t = meth (* full method and without defaults *)
  type sexpable = meth
  let sexp_of_t = sexp_of_meth
  let t_of_sexp = meth_of_sexp
  let compare = compare_meth
end

module MethSet = struct
  include Set.Make(MethKey)

  let compare_items = MethKey.compare
  let map ~f t = fold ~init:empty ~f:(fun acc e -> add acc (f e)) t

  let remove_set ~base:init w =
    fold ~init w ~f:(fun  acc el -> remove acc el)
end

type enum = {
  e_flag_name: string;
  e_name: string;
  e_items: string list;
  e_access: [`Public | `Protected | `Private]
} with sexp 
type constr = func_arg list with sexp
type prop = string * string option * string option with sexp
type sgnl = string * (func_arg list)	with sexp
type clas = { 
  c_inherits: string list;
  c_props: prop list;
  c_sigs: sgnl list;
  c_slots: MethSet.t;
  c_meths: MethSet.t;
  c_enums: enum list;
  c_constrs: constr list;
  c_name: string
}
and namespace = { ns_name:string; ns_classes:clas list; ns_enums:enum list; ns_ns: namespace list }

and slt = meth
with sexp


let empty_namespace = { ns_name="empty"; ns_classes=[]; ns_enums=[]; ns_ns=[] }

(* convert class to pointer on this class *)
let ptrtype_of_class c = 
  { t_name=c.c_name; t_indirections=1; t_is_const=false; t_is_ref = false; t_params=[] }
let ptrtype_of_classname name =
  { t_name=name; t_indirections=1; t_is_const=false; t_is_ref = false; t_params=[] }

let is_void_type t = (t.t_name = "void") && (t.t_indirections=0) 

let meth_of_constr ~classname m_args = 
  let m_declared = classname and m_name=classname and m_out_name=classname
  and m_res={ t_name=classname; t_indirections=1; t_is_ref=false; t_params=[]; t_is_const=false } in
  { m_declared; m_name; m_args; m_res; m_out_name; m_access=`Public; m_modif=`Normal }

let string_of_type t =
  let b = Buffer.create 10 in
  if t.t_is_const then Buffer.add_string b "const ";
  Buffer.add_string b t.t_name;
  Buffer.add_string b (String.make t.t_indirections '*');
  if t.t_is_ref then Buffer.add_string b " &";
  Buffer.contents b

let string_of_arg {arg_type;arg_name;arg_default} =
  let b = Buffer.create 10 in
  Buffer.add_string b (string_of_type arg_type);
  Option.iter arg_name    ~f:(fun name -> Buffer.add_char b ' '; Buffer.add_string b name);
  Option.iter arg_default ~f:(fun v -> Buffer.add_string b " = "; Buffer.add_string b v);
  Buffer.contents b

let string_of_meth m = 
  let args_lst = List.map m.m_args ~f:string_of_arg in
  let args_str = String.concat args_lst ~sep:", " in
  let typ = string_of_type m.m_res in
  if args_lst = []
  then sprintf "%s %s()" typ m.m_name
  else begin
    let space =
      let last = List.last_exn args_lst in
      (* additional space for OCaml compiler (comments) *)
      if '*' = last.[String.length last - 1] then " " else ""
    in
    Printf.sprintf "%s %s(%s%s)" typ m.m_name args_str space
  end

let string_of_constr ~classname c = 
  meth_of_constr ~classname c |> string_of_meth

let rec headl c lst =
  let rec helper c h tl = 
    if c=0 then (List.rev h,tl)
    else match tl with
      | hh::ll -> helper (c-1) (hh::h) ll
      | [] -> raise (Failure "headl")
  in
  helper c [] lst

let skipNs = function
  | "std" | "QGL" | "internal" | "QtConcurrent" | "QtPrivate" | "QDrawBorderPixmap" 
  | "QtSharedPointer" | "QMdi" | "QAlgorithmsPrivate" | "QAccessible2" -> true
  | _ -> false

let fixTemplateClassName x = 
  x |> (Str.global_replace (Str.regexp "&gt;") ">")
    |> (Str.global_replace (Str.regexp "&lt;") "<") 

let str_replace ~patt init = List.fold_left 
  ~f:(fun aggr (patt, v) -> 
    Str.global_replace (Str.regexp patt) v aggr
  ) ~init patt

let strip_dd ~prefix:p s =
  let plen = String.length p+2 in
  if (String.length s < plen) then s 
  else if (Str.first_chars s plen = (p^"::") ) then Str.string_after s plen 
  else s 

let qtFlags = 
  [ ("WindowStates", "WindowState");
	("WindowFlags",  "WindowType");
	("GestureFlags", "GestureFlag");
	("Alignment",    "AlignmentFlag");
	("MouseButtons", "MouseButton")
  ]

let fixEnumName s =
  try
    List.Assoc.find_exn qtFlags s
  with
    | Not_found -> s

let str2policy = function
  | "public" -> `Public
  | "protected" -> `Protected
  | "private" -> `Private
  | _ -> raise (Invalid_argument "str2policy") 

(********************* Parsing code *********************************)
let parse_prop = function
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
let rec parse_arg (_,attr,lst) : func_arg = 
  let arg_default = List.Assoc.find attr "default" in
  let arg_name = List.Assoc.find attr "argname" in
  let t_indirections = List.Assoc.find_exn attr "indirections" |> int_of_string in
  let t_is_ref = List.Assoc.find_exn attr "isReference" |> bool_of_string in
  let t_is_const = List.Assoc.find_exn attr "isConstant" |> bool_of_string in
  let t_name = List.Assoc.find_exn attr "type" |> fixTemplateClassName 
  |> fixEnumName
  in
  let t_params = 
    match lst with 
      | [Element ("arguments",_,lst)] ->
	let foo = function
	  | Element (("argument",_,_) as e) -> (parse_arg e).arg_type
	  | _ -> assert false
	in
	List.map ~f:foo lst
      | _ -> []
  in
  let arg_type = {t_name; t_is_const; t_is_ref; t_indirections; t_params} in
  { arg_type; arg_name; arg_default }

let parse_enum superName node : enum option = match node with
  | ("enum",attr,lst) -> 
    let name = ref (List.Assoc.find attr "name") in
    let mems = ref [] in
    let access = ref `Public in
    List.iter lst ~f:(function
      | Element ("anonymous",_,_) -> name := None
      | Element ("enumerator",attr, [PCData name]) -> mems := (name,List.Assoc.find attr "value") :: !mems
      | Element ("accessPolicy",("value","public")::_,_) -> access := `Public
      | Element ("accessPolicy",("value","private")::_,_) -> access := `Private
      | Element ("accessPolicy",("value","protected")::_,_) -> access := `Protected
      | _ -> assert false);
    let mems = List.rev !mems in
    let mems = filter_enum_members mems in
    let mems = List.filter mems ~f:(fun item ->
      String.fold item ~init:0 ~f:(fun acc c-> acc+(if Char.is_uppercase c then 1 else 0)) < 4
    ) in
    (match !name with
      | _ when List.length mems =0 -> None
      | Some name ->
	let e_name = strip_dd ~prefix:superName name in
	let e_flag_name = try
			    List.Assoc.find_exn (List.Assoc.inverse qtFlags) e_name
	  with Not_found -> e_name in
	let e_items = mems in
	let e_access = !access in
	Some { e_name; e_flag_name; e_items; e_access }
      | None -> None)
  | _ -> assert false
  
let rec build root = match root with
  | PCData x -> raise (Common.Bug "root element of input xml can't be PCData")
  | Element ("code",_,lst) -> 
    let classes = ref [] in
    let nss = ref [] in  (* namespaces *)
    let enums = ref [] in

    List.iter lst ~f:(function
      | Element (("class",attr,lst) as e) ->
	      classes := (parse_class "" e) :: !classes
      | Element (("namespace",_,lst) as e) -> 
	      nss := (parse_ns "" e) :: !nss
      | Element ( ("enum",_,lst) as e) -> begin
	    match parse_enum "" e with
	      | None -> ()
	      | Some e -> Ref.replace enums (fun lst -> e::lst)
      end
      | _ -> raise (Common.Bug "Only `class`, `namespace` or `enum` tags are allwed")
    );
    {ns_name=""; ns_ns= !nss; ns_classes= !classes; ns_enums= !enums }
  | _ -> print_endline "XML file is incorrect";
    assert false
  
and parse_class nsname c  = 
  match c with
  | ("class",attr,lst) ->
    let classname = List.Assoc.find_exn attr "name" |> strip_dd ~prefix:nsname |> fixTemplateClassName in
    
    let helper = 
      let aggr lst = 
	    let args = ref [] in
	    let ret = ref None in
	    let modif  = ref `Normal in
	    let policy = ref `Protected in

	    List.iter lst ~f:(fun x -> match x with
	      | Element (("return",_,_) as e) ->  ret := Some ((parse_arg e).arg_type)
	      | Element ("arguments",_,lst) ->
	          List.iter lst ~f:(function
	            | Element (("argument",_,_) as e) -> args := (parse_arg e) :: !args
	            | _ -> raise (Common.Bug "XML file is incorrect") )

	      | Element ("accessPolicy",("value",p)::_,_) -> policy := str2policy p
	      | Element ("modifiers",_,lst) ->
	          let set = List.map lst ~f:(function Element (n,_,_) -> n | PCData _ -> assert false) in
              modif := if List.mem set "abstract" then `Abstract
		        else if List.mem set "static" then `Static
		        else `Normal
          | PCData _ -> raise (Common.Bug "XML file is incorrect")
          | Element (name,_,_) ->
              raise (Common.Bug (sprintf "Unknown XML tag `%s`" name))
        );
        (List.rev !args, !ret, !policy, !modif)
      in
      function
	    | Element (_,("name",name)::_,lst) ->
	        let (a,b,c,d) = aggr lst in
	        (name,a,b,c,d)
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
      | (Element ("constructor",_,ll)) as e ->
          let (_,args,_,policy,_) = helper e in
          constrs := (args,policy) :: !constrs
      | Element ("inherits",_,ll) ->
	      List.iter ll ~f:(function
	        | Element ("class",("name",nn)::_,_) -> inher := nn :: !inher
	        |  _ -> assert false)
      | (Element ("function",_,ll)) as e -> begin
	    let (name,args,ret,policy,modif) = helper e in 
	    (match ret with
	      | Some r -> mems := (name,args,r,policy,modif) :: !mems
	      | _ -> assert false)
      end
      | (Element ("slot",_,ll)) as e -> begin
	    let (name,args,ret,policy,modif) = helper e in 
        (match ret with
	      | Some r  -> slots := (name,args,r,policy,modif) :: !slots
	      | None -> assert false)
      end
      | (Element ("signal",_,ll)) as e ->
          let (a,b,_,_,_) = helper e in
          sigs := (a,b) :: !sigs
      | Element (("enum",_,_) as e) -> begin
          match (parse_enum classname e) with
	        | None -> ()
	        | Some e -> Ref.replace enums (fun lst -> e::lst) end
      | Element (("property",_,_) as e) -> props := (parse_prop e) :: !props
      | Element ("class",("name",nn)::_,_) ->
	      printf "skipping inner class %s::%s\n" classname (fixTemplateClassName nn)
      | Element ("destructor",_,_) -> ()
      | _ -> assert false
    );

    let f (m_name,m_args,m_res,m_access,m_modif) =
      let m_declared = classname in
      let m_out_name = make_out_name ~classname m_name in
      {m_name; m_args; m_res; m_access; m_modif; m_declared;
       m_out_name }
    in

    let meths = List.map !mems ~f |> MethSet.of_list in
    let slots = List.map !slots ~f |> MethSet.of_list in
      
    let inherits = List.map ~f:fixTemplateClassName !inher in
    (* next remove base classes such as QSet<...>, QList<...>, QVector<...> 
       TODO: add special base classes.
       Also forget inheritance of template base classes
    *)
    let inherits = List.filter inherits ~f:(fun name ->
      if String.is_prefix ~prefix:"QList<" name then false
      else if String.is_prefix ~prefix:"QVector<" name then false
      else if String.is_prefix ~prefix:"QSet<" name then false
      else
        match String.find name ~f:((=) '<') with
	    | Some x -> false | None -> true)
    in
	
    let constrs = List.filter ~f:(fun (args,policy) -> match policy with
      | `Public -> true | `Private | `Protected -> false) !constrs in
    let constrs = List.map ~f:fst constrs in
    { c_name=classname; c_constrs= constrs; c_slots= slots; 
      c_meths= meths;
      c_inherits= inherits; c_enums= !enums; c_props= !props; c_sigs= !sigs }
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
      | Element ( ("enum",_,_) as e) -> begin
	    match parse_enum name e with
	      | Some x -> Ref.replace enums (fun lst -> x::lst)
	      | None -> ()
      end
      | _ -> assert false
    in
    List.iter ~f lst;
    {ns_name=name; ns_classes= !clas; ns_enums= !enums; ns_ns= !nss}
  | _ -> assert false


