open Parser 

let skipClass (c:Parser.clas) =
  let classname = c.c_name in
  match classname with
    | s when isTemplateClass classname -> 
      print_endline ("skipping template class " ^ classname);
      true

    | s when isInnerClass classname -> 
      print_endline ("skipping inner class " ^ classname);
      true

    | "QTabletEvent" -> true (* ??? don't rember why skip it *)
    | _ -> false
   
let skipArgument  = function (* true when don't skip *)
  | "qreal" -> false
  | s when isTemplateClass s -> false
  | s when isInnerClass s -> false
  | _ -> true

exception DoSkip
exception DontSkip
let goodMeth ~classname name res lst access _ = 
  try
    if skipMeth ~classname name then raise DontSkip; 
(*    if startswith ~prefix:"operator" name then raise DontSkip;
    if startswith ~prefix:"d_func" name then raise DontSkip; *)
    
(*    (match modif with 
      | Static | Abstract -> raise Break
      | Virtual -> ()); *)
    (match access with
      | Private | Protected -> raise DontSkip
      | Public -> ());
    let lst = List.map fst lst in
    let lst = res::lst in
    let _ = List.map (fun t -> t.t_name) lst |> List.find skipArgument  in
    false

  with Break | DoSkip -> false
    | DontSkip | Not_found -> true

exception BreakS of string

type t1 = string and t2 = string 
and castResult = Success of t1 | CastError of t2 | CastValueType of t2 | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult

type pattern = InvalidPattern | PrimitivePattern | ObjectPattern | EnumPattern

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

  method private pattern t = 
    let name = t.t_name in
    let indir = t.t_indirections in
    if indir>1 then InvalidPattern 
    else if isTemplateClass name then InvalidPattern
    else match name with
      | "int"  | "bool" | "QString" | "void" -> 
	if indir = 0 then PrimitivePattern else InvalidPattern
      | "qreal" | "double" | "float" -> InvalidPattern
      | s when indir = 1 -> ObjectPattern
      | s when indir > 1 -> InvalidPattern
      | _ -> try
	       if Index.isEnum name self#index then EnumPattern
	       else if Index.isClass name self#index then InvalidPattern
	       else ( print_endline "you cant see this line. attempt to cast namespace";
		      assert false )
	with
	    NotInIndex name -> (print_endline ("!!! Not in index: " ^ name);
				InvalidPattern )


  method private fromCamlCast 
    : indexItem Index.t -> cpptype -> ?default:string option -> string -> castResult
      = fun index t ?(default=None) (argname(*arg name*):string) -> 
	let _ = default in
	let tname = t.t_name in
	let is_const = t.t_is_const in
	match self#pattern t with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (self#type2str t) )
	  | PrimitivePattern ->
 	    (match t.t_name with
	      | "int" -> Success (String.concat "" 
				    ["int _";   argname; " = Int_val("; argname; ");"])
	      | "double" -> CastError "double_value!"
	      | "bool" -> Success (String.concat "" 
				     ["bool _"; argname; " = Bool_val("; argname; ");"])
	      | "QString" -> Success (String.concat "" 	  
					["  "; if t.t_is_const then "const " else "";
					 "QString"; if t.t_is_ref then "&" else "";
					 " _"; argname; " = "; "QString(String_val("; argname; "));" ])
	      | "void" -> Success "Val_unit"
	      | _ -> assert false)

	  | ObjectPattern -> Success (String.concat ""
					[if is_const then "const " else ""; tname; "* _"; argname; 
					 " = (";tname;"*)"; argname; ";"])

	  | EnumPattern -> CastError ("cant't cast enum: " ^ (self#type2str t)) 
	      

  method private toCamlCast 
      = fun t arg ansVarName -> 
	match self#pattern t with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (self#type2str t))
	  | PrimitivePattern ->
	    (match t.t_name with
	      | "int" -> Success (String.concat "" [ansVarName;" = Val_int("; arg; ");"])
	      | "double" -> CastError "Store_double_value!"
	      | "bool" -> Success (String.concat "" [ansVarName; " = Val_bool("; arg; ");"])
	      | "QString" -> Success (String.concat "" [ansVarName; " = caml_copy_string("; arg;
							".toLocal8Bit().data() );"])
	      | _ -> assert false)
	  | ObjectPattern -> Success (ansVarName ^ " = (value)(" ^ arg  ^ ");")
	  | EnumPattern   -> CastError ("cant't cast enum: " ^ (self#type2str t)) 
	
  method type2str  =  Parser.type2str
(*    String.concat "" 	  
      [if t.t_is_const then "const " else "";
       t.t_name; String.make t.t_indirections '*';" "; if t.t_is_ref then "&" else ""]
*)
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
(*
  method buildIndex nslist = 
*)  
end
