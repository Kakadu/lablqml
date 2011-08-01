open Parser 

let skipClass c =
  let classname = c.c_name in
  match classname with
    | s when isTemplateClass classname -> 
      print_endline ("skipping template class " ^ classname);
      true
    | s when Str.split (Str.regexp "::") s |> List.length > 1 -> 
      print_endline ("skipping inner class " ^ classname);
      true

    | "QTabletEvent" -> true
    | _ -> false
   
let skipArgument  = function
  | "qreal" -> false
  | s when Parser.isTemplateClass s -> false
  | _ -> true

let goodMeth name (res: Parser.cpptype option) lst access _ = 
  try
    if skipMeth name then raise Break;
    if startswith ~prefix:"operator" name then raise Break;
    
(*    (match modif with 
      | Static | Abstract -> raise Break
      | Virtual -> ()); *)
    (match access with
      | Private | Protected -> raise Break
      | Public -> ());
    let lst = List.map fst lst in
    let lst = match res with Some x -> x::lst | None -> lst in
    let _ = List.map (fun t -> t.t_name) lst |> List.find skipArgument  in
    false

  with Break -> false
    | Not_found -> true

exception BreakS of string

type t1 = string and t2 = string
and castResult = Success of t1 | CastError of t2
exception BreakOk of t1
exception BreakFail of t2

class virtual abstractGenerator _index = object (self)
    
  method private virtual prefix : string  
  method private virtual genClass : string -> clas -> string option
  method private virtual genEnumOfNs : string -> enum -> string option
  method private virtual genEnumOfClass : string -> out_channel -> enum -> unit
  method private virtual genSlot  : string -> out_channel -> slt -> unit
  method private virtual genSignal : string -> out_channel -> sgnl -> unit
  method private virtual genProp : string -> out_channel -> prop -> unit
  method private virtual genMeth : string -> out_channel -> meth -> unit
  method private virtual genConstr : string -> out_channel -> constr -> unit
  method private virtual makefile : string -> string list -> unit

  method private index : Parser.indexItem Index.t = _index

  method private fromCamlCast 
    : indexItem Index.t -> cpptype -> ?default:string option -> string -> castResult
      = fun index t ?(default=None) (name(*arg name*):string) -> 
	let _ = default in
	let indirections = t.t_indirections in
	let tname = t.t_name in
	let is_ref = t.t_is_ref in
	let is_const = t.t_is_const in
	try
	  if t.t_indirections > 1 then 
	    raise (BreakFail "indirections > 1");

	let ans = (* empty list if can't cast *)
	if (t.t_name = "QString") then begin
	  if indirections<>0 then raise (BreakFail "QString with indirections");
	  ["  "; if t.t_is_const then "const" else "";
	   "QString"; if t.t_is_ref then "&" else "";
	   " _"; name; " = "; "QString(String_val("; name; "));"]	 
	end 
	else if t.t_name = "int" then begin
	  if indirections<>0 then raise (BreakFail "int with indirections");
	  assert (t.t_indirections = 0);  
	  ["int _";   name;" = Int_val(";name; ");"]
	end 
	else if t.t_name = "double" then begin
	  if indirections<>0 then raise (BreakFail "double with indirections");
	  ["double _";name;" = Double_val(";name; ");"]
	end 
	else if t.t_name = "bool" then begin
	  if indirections<>0 then raise (BreakFail "bool with indirections");
	  assert (t.t_indirections = 0);  
	  ["bool _";  name;" = Bool_val(";name; ");"]
	end 
	else if (t.t_indirections = 1) then begin
	  if not (Index.isClass tname (self#index)) then
	    raise (BreakFail ("Not a class has indirections:" ^ (self#type2str t) ));
	  if is_ref then
	    raise (BreakFail ("Class pointer is a reference:" ^ (self#type2str t) ));
	  [if is_const then "const " else ""; tname; "* _"; name; 
	   " = (";tname;"*)"; name; ";"]
	    
	end else if Index.isEnum tname (self#index) then 
	  raise (BreakFail ("enums are not supported (" ^ tname ^ ")"))	    
	else 
	  raise (BreakFail ("some shit (" ^ (self#type2str t) ^ ")"))	   	    
(*	else
	  BreakFail "this is incompilable yet" *)
	in

	if List.length ans > 0 then Success (String.concat "" ans)
	else CastError "impossible string"
	with
	  | BreakOk s -> Success s
	  | BreakFail s -> CastError s
	  | NotInIndex name -> CastError ("Not in index: " ^ name)

  method private toCamlCast 
    : indexItem Index.t -> cpptype -> ?default:string option -> string -> castResult
      = fun index t ?(default=None) (name(*arg name*):string) -> 


  method type2str t = 
    String.concat "" 	  
      [if t.t_is_const then "const " else "";
       t.t_name; String.make t.t_indirections '*';" "; if t.t_is_ref then "&" else ""]


  method cppFuncName classname funcname (argslist: func_arg list) = 
    String.concat "_" ("ml"::classname::funcname::(
      argslist |> List.map (fun (t,_) -> t.t_name)
    ))

  method genNs dir ns =     
    if not (skipNs ns.ns_name) then begin 
      let iter = List.iter in
      let dir  = dir ^ ns.ns_name in
      if Sys.file_exists dir then
	ignore (Sys.command ("rm -rf " ^ dir) );
      Unix.mkdir dir 0o755;
      let classes = ref [] in (* classes and enums *)
      let wrap = function
	| Some name -> classes := name :: !classes | None -> ()
      in
      iter (self#genNs (self#prefix)) ns.ns_ns;
      iter (fun c -> self#genClass dir c |> wrap) ns.ns_classes;
      iter (fun e -> self#genEnumOfNs dir e |> wrap) ns.ns_enums;
      self#makefile dir !classes;
    end
  method generate = (self#prefix) |> self#genNs |> List.iter
end
