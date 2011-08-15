open Parser 
open Core
open Core.Common
open Printf
module List = Core_list
module String = Core_string
open SuperIndex

exception BreakS of string

let breaks s = raise (BreakS s)

let cpp_func_name ~classname ~methname argslist = 
  String.concat ~sep:"_" ("ml"::classname::methname::(
    List.map argslist ~f:(fun (t,_) -> t.t_name)
  ))

let isTemplateClass name = 
    try ignore (String.index_exn name '<' : int); true
    with Not_found -> false

let isInnerClass name = 
  try let _ = Str.search_forward (Str.regexp "::") name 0 in true
  with Not_found -> false

let skipMeth ~classname name = 
  if startswith ~prefix:"operator" name then true
  else if startswith ~prefix:"d_func" name then true
  else match name with
    | "flush" 	
    | "initialize" (* QAccessible *) -> true (* because preprocesser has its own `flush` *)
    | "findChild" when classname = "QWidget" -> (*TODO: add into xml generator info about generic methods *)
      true
      
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
    | "toGraphicsObject" (* when classname = "QGraphicsItem" *) -> true (* very strange overrides *)
    | _ when classname = "QMapData" -> true (* TODO: undestand this class. maybe dont generate it *)
    | "QThreadStorageData::QThreadStorageData" -> true
    | "QThreadStorageData" -> true (* cause it has a function-pointer parameter *)
    | _ -> false

let skipClass c =
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
   
let skipArgument arg = match arg.t_name with (* true when do skip *)
  | "GLfloat" | "GLint" | "GLuint"
  | "void" | "uchar"
  | "qreal" -> true
  | s when isTemplateClass s -> true
  | s when isInnerClass s -> true
  | _ -> false
    
let skip_ns ~prefix _ = false


exception DoSkip
exception DontSkip

let is_good_meth ~classname m = 
  let methname = m.m_name in
  let args = m.m_args in
  let res = m.m_res in
  try
    if skipMeth ~classname methname then false 
    else begin 
      let lst = List.map ~f:fst args in
      match List.find lst ~f:skipArgument with
	| None -> (* all arguments are OK *)
	  (is_void_type res) or (not (skipArgument res))
	| Some x -> (printf "Method %s skipped: %s\n" methname (string_of_type x); false)
    end
  with DoSkip -> false
    | DontSkip | Not_found -> true

type t1 = string and t2 = string 
and castResult = Success of t1 | CastError of t2 | CastValueType of t2 | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult

type pattern = InvalidPattern | PrimitivePattern | ObjectPattern | EnumPattern

let  pattern index t = 
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
      | s when indir = 1 -> 
	let key = NameKey.key_of_fullname name in
	if SuperIndex.mem index key  then ObjectPattern
	else InvalidPattern
      | s when indir > 1 -> InvalidPattern
      | _ -> try
	       let key = NameKey.key_of_fullname name in
	       if is_enum_exn ~key index then EnumPattern
	       else if is_class_exn ~key index then InvalidPattern
	       else ( print_endline "you cant see this line. some fucking shit";
		      assert false )
	with
	    Not_found  -> (print_endline ("!!! Not in index: " ^ name);
				InvalidPattern )

let is_abstract_class ~prefix index name = 
    let key = NameKey.make_key ~prefix ~name in
    match SuperIndex.find index key with
      | Some (Class (_,set)) when MethSet.is_empty set -> false
      | Some (Class _) -> true
      | None -> raise (Common.Bug (sprintf "Class %s is not in index" name))
      | Some (Enum _) -> raise (Common.Bug (sprintf "expected class %s, but enum found" name) )    


class virtual abstractGenerator _index = object (self)
  method private index = _index    
  method private virtual prefix : string  
  method private virtual gen_class : prefix:string list -> dir:string -> clas -> string option
  method private virtual gen_enumOfNs : prefix:string list -> dir:string -> enum -> string option
  method private virtual gen_enumOfClass : string -> out_channel -> enum -> unit
  method private virtual genSlot  : string -> out_channel -> slt -> unit
  method private virtual genSignal : string -> out_channel -> sgnl -> unit
  method private virtual genProp : string -> out_channel -> prop -> unit
  method private virtual genMeth : string -> out_channel -> meth -> unit
  method private virtual genConstr : string -> out_channel -> constr -> unit
  method private virtual makefile : string -> string list -> unit


  method private fromCamlCast 
    : index_data SuperIndex.t -> cpptype -> ?default:string option -> string -> castResult
      = fun index t ?(default=None) (argname(*arg name*):string) -> 
	let _ = default in
	let tname = t.t_name in
	let is_const = t.t_is_const in
	match pattern _index t with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t) )
	  | PrimitivePattern ->
 	    (match t.t_name with
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
	      | _ -> assert false)

	  | ObjectPattern -> Success (String.concat 
					[if is_const then "const " else ""; tname; "* _"; argname; 
					 " = (";tname;"*)"; argname; ";"])

	  | EnumPattern -> CastError ("cant't cast enum: " ^ (string_of_type t)) 
	      

  method private toCamlCast 
      = fun t arg ansVarName -> 
	match pattern _index t with
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
	  | ObjectPattern -> Success (ansVarName ^ " = (value)(" ^ arg  ^ ");")
	  | EnumPattern   -> CastError ("cant't cast enum: " ^ (string_of_type t)) 

  (* prefix is namespace preifx (reversed) 
     dir is directory where create subdirectory
  *)
  method gen_ns ~prefix ~dir ns =     
    if not (skip_ns ~prefix ns.ns_name) then begin 
      let dir = dir ^/ ns.ns_name in
      if Sys.file_exists dir then
	ignore (Sys.command ("rm -rf " ^ dir) );
      ignore (Sys.command ("mkdir -p -m 755 " ^ dir));
(*      Unix.mkdir dir 0o755; (* line above is recursive mkdir *) *)
      let classes = ref [] in (* classes and enums which was successfully generated *)
      let wrap = function
	| Some name -> classes := name :: !classes | None -> ()
      in
      let prefix = ns.ns_name :: prefix in
      List.iter ~f:(self#gen_ns ~prefix ~dir) ns.ns_ns;
      List.iter ~f:(fun c -> self#gen_class ~prefix ~dir c |> wrap) ns.ns_classes;
      List.iter ~f:(fun e -> self#gen_enumOfNs ~prefix ~dir e |> wrap) ns.ns_enums;
      self#makefile dir !classes;
    end

  (* root namespace needs special magic (it doesn't have name) *)
  method generate ns = 
    let dir = self#prefix in
    if Sys.file_exists dir then
      ignore (Sys.command ("rm -rf " ^ dir) );
    ignore (Unix.mkdir dir 0o755);
    let classes = ref [] in
    let wrap = Option.iter ~f:(fun name -> classes:= name :: !classes) in
    let prefix = [] in
    List.iter ~f:(self#gen_ns ~prefix ~dir) ns.ns_ns;
    List.iter ~f:(fun c -> self#gen_class ~prefix ~dir c |> wrap) ns.ns_classes;
    List.iter ~f:(fun e -> self#gen_enumOfNs ~prefix ~dir e |> wrap) ns.ns_enums;
    self#makefile dir !classes
    

(*    let curdir = self#prefix in
    SuperIndex.iter index ~f:(fun ~key ~data -> match data with
      | Enum e -> self#gen_enum curdir e
      | Class (c,lst) ->
	let is_abstract = List.length lst > 0 in
	self#gen_class ~is_abstract curdir c
    );
    (self#prefix) |> self#genNs |> List.iter
      *)
end
