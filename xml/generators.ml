open Parser 
open Core
open Core.Common
open Printf
module List = Core_list
module String = Core_string
open SuperIndex

exception BreakS of string
exception BreakSilent
let breaks s = raise (BreakS s)

let cpp_func_name ~classname ~methname argslist = 
  String.concat ~sep:"_" ("ml"::classname::methname::(
    List.map argslist ~f:(fun (t,_) -> Str.global_replace (Str.regexp "::") "_" t.t_name)
  ))

let isTemplateClass name = 
    try ignore (String.index_exn name '<' : int); true
    with Not_found -> false

let isInnerClass name = 
  try let _ = Str.search_forward (Str.regexp "::") name 0 in true
  with Not_found -> false

let skip_class_by_name ~classname =
  match classname with
    | s when isTemplateClass classname -> 
      print_endline ("skipping template class " ^ classname);
      true
    | s when isInnerClass classname -> 
      print_endline ("skipping inner class " ^ classname);
      true
    | "QWindowsVistaStyle"  | "QWindowsXPStyle" | "QWindowsStyle" | "QWindowsCEStyle"
    | "QWindowsMobileStyle" | "QS60Style" -> true
    | "QString" -> true (* because we implemented it as primitive *)
    | _ -> false

let skipClass ~prefix c = 
  match prefix with
    | _ when skip_class_by_name c.c_name -> true
    | [] -> false
    | lst when List.hd_exn (List.rev lst) = "QtConcurrent" -> true
    | _ -> false

type pattern = 
  | InvalidPattern | PrimitivePattern | ObjectPattern 
  | EnumPattern  of enum * NameKey.t
  | ObjectDefaultPattern (* when `=0` *) (* default pattern when parameter has default value *)

let pattern index (t,default) =
    let name = t.t_name in
    let indir = t.t_indirections in
    if indir>1 then InvalidPattern 
    else if isTemplateClass name then InvalidPattern
    else match name with
      | "int"  | "bool" | "QString" | "void" -> 
	if indir = 0 then PrimitivePattern else InvalidPattern
      | "char" when indir = 1 -> PrimitivePattern
      | "char"
      | "qreal" | "double" | "float" -> InvalidPattern
      | s when indir = 1 -> begin
	let key = NameKey.key_of_fullname name in
	match SuperIndex.find index key with
	  | Some (Class _) when default=Some "0" -> ObjectDefaultPattern
	  | Some (Class _)  -> ObjectPattern
	  | Some (Enum _)
	  | None  -> InvalidPattern
      end
      | s when indir > 1 -> InvalidPattern
      | _ -> begin
	let key = NameKey.key_of_fullname name in
	match SuperIndex.find index key  with
	  | Some (Class _) -> InvalidPattern
	  | Some (Enum e) -> EnumPattern (e, key)
	  | None -> (print_endline ("!!! Not in index: " ^ name); InvalidPattern)
      end
   
let skipArgument ~index ((t,_) as arg) =  (* true when do skip *)
    match t.t_name with
      | "GLfloat" | "GLint" | "GLuint"
      | "void" | "uchar"
      | "qreal" -> true
      | s when isTemplateClass s -> true
      | _ -> begin
	match pattern index arg with
	  | EnumPattern _
	  | ObjectDefaultPattern
	  | ObjectPattern -> 
	    let key = NameKey.key_of_fullname t.t_name in
	    not (SuperIndex.mem index key) 
	  | _ -> false
      end
    
exception DoSkip
exception DontSkip

let is_good_meth ~classname ~index m = 
  let methname = m.m_name in
  let args = m.m_args in
  let res = m.m_res in
  try
    if skip_class_by_name ~classname:m.m_declared then false
    else if skip_meth ~classname methname then false 
    else begin 
      match List.find args ~f:(skipArgument ~index) with
	| None -> (* all arguments are OK *)
	  (is_void_type res) or (not (skipArgument ~index (res,None) ))
	| Some (x,_) -> (printf "Method %s skipped: %s\n" methname (string_of_type x); false)
    end
  with DoSkip -> false
    | DontSkip | Not_found -> true

type t1 = string and t2 = string 
and castResult = Success of t1 | CastError of t2 | CastValueType of t2 | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult

let enum_conv_func_names (lst,_) = 
  let f s lst = String.concat ~sep:"_" (s :: (List.rev lst)) in
  (f "enum_of_caml" lst, f "enum_to_caml" lst)

let is_abstract_class ~prefix index name = 
  let key = NameKey.make_key ~prefix ~name in
  let f = fun m acc -> match m.m_modif with `Abstract -> true | _ -> acc in
  match SuperIndex.find index key with
    | Some (Class (c,_)) -> 
      (MethSet.fold ~init:false c.c_meths ~f) or ((MethSet.fold ~init:false c.c_slots ~f))
    | None -> raise (Common.Bug (sprintf "Class %s is not in index" name))
    | Some (Enum _) -> raise (Common.Bug (sprintf "expected class %s, but enum found" name) )    

class virtual abstractGenerator _index = object (self)
  method private index = _index    
  method private virtual prefix : string  
  method private virtual gen_class : prefix:string list -> dir:string -> clas -> string option
  method private virtual gen_enum_in_ns : key:NameKey.t -> dir:string -> enum -> string option
  method private virtual genProp : string -> out_channel -> prop -> unit
  method private virtual genMeth : prefix:string list -> string -> out_channel -> meth -> unit
  method private virtual genConstr : prefix:string list -> string -> out_channel -> constr -> unit
  method private virtual makefile : string -> string list -> unit


  method private fromCamlCast 
    : index_data SuperIndex.t -> cpptype -> default:string option -> string -> castResult
      = fun index t ~default (argname:string) -> 
	let _ : string option = default in	
	let is_const = t.t_is_const in
	match pattern _index (t,default) with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t) )
	  | PrimitivePattern -> begin
 	    match t.t_name with
	      | "int" -> Success (String.concat  
				    ["int _";   argname; " = Int_val("; argname; ");"])
	      | "double" -> CastError "double_value!"
	      | "bool" -> Success (String.concat 
				     ["bool _"; argname; " = Bool_val("; argname; ");"])
	      | "QString" -> Success (String.concat 
					["  "; if t.t_is_const then "const " else "";
					 "QString"; if t.t_is_ref then "&" else "";
					 " _"; argname; " = "; "QString(String_val("; argname; "));" ])
	      | "void" -> Success "Val_unit"
	      | "char" when t.t_indirections = 1 ->
		Success (String.concat ["char* _"; argname; " = String_val("; argname; ");"])
	      | _ -> assert false
	  end
	  | ObjectPattern -> 
	    Success (String.concat 
		       [if is_const then "const " else ""; t.t_name; "* _"; argname; 
			sprintf " = (%s* ) (%s);" t.t_name argname])
	  | ObjectDefaultPattern ->
	    Success 
	      (String.concat 
		 [if is_const then "const " else ""; t.t_name; "* _"; argname; 
		  sprintf " = (%s==Val_none) ? NULL : ((%s* )(Some_val(%s)));" argname t.t_name argname] )
	  | EnumPattern (e,k) ->
	    let func_name = enum_conv_func_names k |> fst in
	    let tail = sprintf "%s(%s);" func_name argname in
	    let ans = sprintf "%s _%s = %s" (snd k) argname tail in
	    Success ans	      

  method private toCamlCast
      = fun ?(forcePattern=None) (t,default) arg ansVarName ->
	let patt = match forcePattern with
	  | Some x -> x
	  | None -> pattern _index (t,default) in
	match patt with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t))
	  | PrimitivePattern ->
	    (match t.t_name with
	      | "int" -> Success (String.concat [ansVarName;" = Val_int("; arg; ");"])
	      | "double" -> CastError "Store_double_value!"
	      | "bool" -> Success (String.concat [ansVarName; " = Val_bool("; arg; ");"])
	      | "QString" -> Success (String.concat [ansVarName; " = caml_copy_string("; arg;
							".toLocal8Bit().data() );"])
	      | "char" when t.t_indirections=1 ->
		Success (String.concat [ansVarName; " = caml_copy_string(";arg;");"])
	      | _ -> assert false)
	  | ObjectPattern ->        Success (sprintf "%s = (value)(%s);" ansVarName arg)
	  | ObjectDefaultPattern -> Success (sprintf "%s = Val_some((value)(%s));" ansVarName arg)
	  | EnumPattern (e,k) -> 
	    let func_name = enum_conv_func_names k |> snd in
	    Success (sprintf "%s = %s(%s);" ansVarName func_name arg)

end
