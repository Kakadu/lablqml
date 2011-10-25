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

let cpp_stub_name ~classname ?res_n_name ?(is_byte=true) modif args =
  let argslist = List.map args ~f:(fun {arg_type=t;_}-> Str.global_replace (Str.regexp "::") "_" t.t_name) in
  let sort = if is_byte then "byte" else "native" in
  let modifstr = match modif with `Public -> "pub" | `Private -> "private" | `Protected -> "prot" in
  match res_n_name with
    | Some (res_t,name) -> String.concat ~sep:"_" (sort::modifstr::classname::name::argslist)
    | None              -> String.concat ~sep:"_" (sort::modifstr::"createeee"::classname::argslist)

let isTemplateClass name = 
  try ignore (String.index_exn name '<' : int); true
  with Not_found -> false

let isInnerClass name = 
  try let _ : int = Str.search_forward (Str.regexp "::") name 0 in true
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
with sexp

let pattern index {arg_type=t; arg_default=default; _} =
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
	  | None -> 
	    printf "Warning. Not in index: %s. skipped.\n"  name; 
	    InvalidPattern
      end
   
let skipArgument ~index ({arg_type; arg_name; arg_default} as arg) =  (* true when do skip *)
    match arg_type.t_name with
      | "GLfloat" | "GLint" | "GLuint"
      | "void" | "uchar"
      | "qreal" -> true
      | s when isTemplateClass s -> true
      | _ -> begin
	match pattern index arg with
	  | EnumPattern _
	  | ObjectDefaultPattern
	  | ObjectPattern -> 
	    let key = NameKey.key_of_fullname arg_type.t_name in
	    not (SuperIndex.mem index key) 
	  | InvalidPattern -> true
	  | _ -> false
      end
    
exception DoSkip
exception DontSkip

let is_good_meth ~classname ~index m = 
  let methname = m.m_name in
  let args = m.m_args in
  let res = m.m_res in
  try
    let () = match m.m_access with `Private -> raise DoSkip | `Public | `Protected -> () in
    if skip_class_by_name ~classname:m.m_declared then false
    else if skip_meth ~classname methname then false 
    else begin 
      match List.find args ~f:(skipArgument ~index) with
	| None -> (* all arguments are OK *)
	  (is_void_type res) or (not (skipArgument ~index (simple_arg res) ))
	| Some {arg_type;_} -> (printf "Method %s skipped: %s\n" methname (string_of_type arg_type); false)
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
	or (c.c_constrs = [] )
    | None -> raise (Common.Bug (sprintf "Class %s is not in index" name))
    | Some (Enum _) -> raise (Common.Bug (sprintf "expected class %s, but enum found" name) )    

class virtual abstractGenerator _index = object (self)
  method private index = _index    
  method private virtual prefix : string  
  method private virtual gen_class : prefix:string list -> dir:string -> clas -> string option
  method private virtual gen_enum_in_ns : key:NameKey.t -> dir:string -> enum -> string option

(* TODO: decide what index to use: from object or from parameter *)
  method private fromCamlCast 
      = fun (index:index_t) ({arg_default; arg_type=t; _} as arg) ?(cpp_argname=None) arg_name -> 
	let cpp_argname = match cpp_argname with
	  | Some x -> x 
	  | None   -> "_"^arg_name
	in
	let is_const = t.t_is_const in
	match pattern _index arg with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t) )
	  | PrimitivePattern -> begin
 	    match t.t_name with
	      | "int" -> Success (String.concat  
				    ["int "; cpp_argname; " = Int_val("; arg_name; ");"])
	      | "double" -> CastError "double_value!"
	      | "bool" -> Success (String.concat 
				     ["bool "; cpp_argname; " = Bool_val("; arg_name; ");"])
	      | "QString" -> Success (String.concat 
					["  "; if t.t_is_const then "const " else "";
					 "QString"; if t.t_is_ref then "&" else "";
					 " "; cpp_argname; " = "; "QString(String_val("; arg_name; "));" ])
	      | "void" -> Success "Val_unit"
	      | "char" when t.t_indirections = 1 ->
		Success (String.concat ["char* "; cpp_argname; " = String_val("; arg_name; ");"])
	      | _ -> assert false
	  end
	  | ObjectPattern -> 
	    Success (String.concat 
		       [if is_const then "const " else ""; t.t_name; "* "; cpp_argname; 
			sprintf " = (%s* ) (%s);" t.t_name arg_name])
	  | ObjectDefaultPattern ->
	    Success 
	      (String.concat 
		 [if is_const then "const " else ""; t.t_name; "* "; cpp_argname; 
		  sprintf " = (%s==Val_none) ? NULL : ((%s* )(Some_val(%s)));" arg_name t.t_name arg_name] )
	  | EnumPattern (e,k) ->
	    let func_name = enum_conv_func_names k |> fst in
	    let tail = sprintf "%s(%s);" func_name arg_name in
	    let ans = sprintf "%s %s = %s" (snd k) cpp_argname tail in
	    Success ans	      

  method private toCamlCast
      = fun ?(forcePattern=None) {arg_type=t;arg_default=default;_} arg ansVarName ->
	let patt = match forcePattern with
	  | Some x -> x
	  | None -> pattern _index (simple_arg t) in
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
	      | _ -> raise (Common.Bug (sprintf "unexpected primitive: %s" t.t_name))
	    )
	  | ObjectPattern ->        Success (sprintf "%s = (value)(%s);" ansVarName arg)
	  | ObjectDefaultPattern -> Success (sprintf "%s = Val_some((value)(%s));" ansVarName arg)
	  | EnumPattern (e,k) -> 
	    let func_name = enum_conv_func_names k |> snd in
	    Success (sprintf "%s = %s(%s);" ansVarName func_name arg)

end
