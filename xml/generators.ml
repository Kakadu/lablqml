open Parser 
open Core
open Core.Common
open Core.Std
open Printf
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
  try let (_ : int) = Str.search_forward (Str.regexp "::") name 0 in true
  with Not_found -> false

let skip_class_by_name ~classname =
  match classname with
    | s when isTemplateClass classname -> 
      print_endline ("skipping template class " ^ classname);
      true
    | s when isInnerClass classname -> 
      print_endline ("skipping inner class " ^ classname);
      true
    | "QString" -> true (* because we implemented it as primitive *)
    | _ -> false

let skipClass ~prefix c = 
  match prefix with
    | _ when skip_class_by_name c.c_name -> true
    | [] -> false
    | lst when List.hd_exn (List.rev lst) = "QtConcurrent" -> true
    | _ -> false

type pattern = 
  | InvalidPattern 
  | PrimitivePattern of string
  | ObjectPattern 
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
	      if indir = 0 then PrimitivePattern name else InvalidPattern
      | "char" when indir = 1 -> PrimitivePattern "char*"
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
(*
	    printf "Warning. Not in index: %s. skipped.\n"  name; *)
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

let is_good_meth ~classname ~index {m_args; m_res; m_access; m_declared; _} =
  try
    let () = match m_access with `Private -> raise DoSkip | `Public | `Protected -> () in
    if skip_class_by_name ~classname:m_declared then false
    else begin 
      match List.find m_args ~f:(skipArgument ~index) with
	| None -> (* all arguments are OK *)
	  (is_void_type m_res) or not (skipArgument ~index (simple_arg m_res))
	| Some {arg_type;_} -> 
        (*printf "Method %s skipped: %s\n" methname (string_of_type arg_type); *)
        false
    end
  with DoSkip -> false
    | DontSkip | Not_found -> true

type t1 = string and t2 = string 
and castResult = Success of t1 | CastError of t2 | CastValueType of t2 | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult

let enum_conv_func_names (lst,_) = 
  let s = String.concat ~sep:"_" (List.rev lst) in
  ("enum_of_caml_"^s, "enum_to_caml_"^s)

let is_abstract_class ~prefix index name =
  let key = NameKey.make_key ~prefix ~name in
  let f = fun acc m -> match m.m_modif with 
    | `Abstract -> 
        printf "Abstract meth found in class %s: %s\n%!" name (string_of_meth m);
        true
    | _ -> acc in
  let ans = match SuperIndex.find index key with
    | Some (Class (c,_)) -> 
      (MethSet.fold ~init:false c.c_meths ~f) or ((MethSet.fold ~init:false c.c_slots ~f))
	    or (c.c_constrs = [])
    | None -> raise (Common.Bug (sprintf "Class %s is not in index" name))
    | Some (Enum _) -> raise (Common.Bug (sprintf "expected class %s, but enum found" name) )    
  in
  printf "is_abstract_class of %s says %b\n" (NameKey.to_string key) ans;
  ans

let pathChecker graph =
  let module M = Graph.Path.Check(G) in
  let checker = M.create graph in
  M.check_path checker

let isQObject ~key graph =
  let qObjectKey = NameKey.key_of_fullname "QObject" in
  if qObjectKey = key then true else
  if not (G.mem_vertex graph qObjectKey) then false
  else pathChecker graph qObjectKey key

  (* TODO: decide what index to use: from object or from parameter *)
  (* TODO: rewrite function to return type Core.Common.passfail *)
  let fromCamlCast 
      = fun (index:index_t) ({arg_default; arg_type=t; _} as arg) ?(cpp_argname=None) arg_name -> 
	let cpp_argname = match cpp_argname with
	  | Some x -> x 
	  | None   -> "_"^arg_name
	in
	let is_const = t.t_is_const in
	match pattern index arg with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t) )
	  | PrimitivePattern "int" ->
          Success (sprintf "int %s = Int_val(%s);" cpp_argname arg_name)
	  | PrimitivePattern "double" -> CastError "double_value!"
	  | PrimitivePattern "bool" ->
          Success (sprintf "bool %s = Bool_val(%s);" cpp_argname arg_name)
	  | PrimitivePattern "QString" ->
          Success (String.concat 
					 ["  "; if t.t_is_const then "const " else "";
					  "QString"; if t.t_is_ref then "&" else "";
					  " "; cpp_argname; " = "; "QString(String_val("; arg_name; "));" ])
	  | PrimitivePattern "void" -> Success "Val_unit"
	  | PrimitivePattern "char*" ->
          Success (sprintf "char* %s = String_val(%s);" cpp_argname arg_name)
      | PrimitivePattern _ -> assert false
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

  let toCamlCast
      = fun ~index ?(forcePattern=None) {arg_type=t;arg_default=default;_} arg ansVarName ->
	let patt = match forcePattern with
	  | Some x -> x
	  | None -> pattern index (simple_arg t) in
	match patt with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t))
	  | PrimitivePattern "int" -> Success (String.concat [ansVarName;" = Val_int("; arg; ");"])
	  | PrimitivePattern "double" -> CastError "Store_double_value!"
	  | PrimitivePattern "bool" -> Success (String.concat [ansVarName; " = Val_bool("; arg; ");"])
	  | PrimitivePattern "QString" ->
          Success (String.concat [ansVarName; " = caml_copy_string("; arg; ".toLocal8Bit().data() );"])
	  | PrimitivePattern "char*" -> Success (String.concat [ansVarName; " = caml_copy_string(";arg;");"])
	  | PrimitivePattern _ -> raise (Common.Bug (sprintf "unexpected primitive: %s" t.t_name))
	  | ObjectPattern ->        Success (sprintf "%s = (::value)(%s);" ansVarName arg)
	  | ObjectDefaultPattern -> Success (sprintf "%s = Val_some((::value)(%s));" ansVarName arg)
	  | EnumPattern (e,k) -> 
	    let func_name = enum_conv_func_names k |> snd in
	    Success (sprintf "%s = %s(%s);" ansVarName func_name arg)

(* I don't understand how to use QAbstractItemView  because of method
 * virtual QModelIndex 	indexAt(const QPoint & point) const = 0
 * Class QModelIndex has only 1 default constructor and I don't know how to use it properly
*)
let does_need_twin ~isQObject classname =
  isQObject &&
    (match classname with
      | "QAbstractItemView" -> false
      | _ -> true
    )
