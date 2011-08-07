open Parser 
open Core
module List = Core_list
module String = Core_string
open SuperIndex

let cpp_func_name ~classname ~methname argslist = 
  String.concat ~sep:"_" ("ml"::classname::methname::(
    List.map argslist ~f:(fun (t,_) -> t.t_name)
  ))

let isTemplateClass name = 
    try ignore (String.index name '<'); true
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
    | "toGraphicsObject" when classname = "QGraphicsItem" -> true (* very strange overrides *)
    | _ when classname = "QMapData" -> true (* TODO: undestand this class. maybe dont generate it *)
    | "QThreadStorageData::QThreadStorageData" -> true
    | "QThreadStorageData" -> true (* cause it has a function-pointer parameter *)
    | _ -> false


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
  | "void" -> false
  | s when isTemplateClass s -> false
  | s when isInnerClass s -> false
  | _ -> true
    
let skip_ns _ = false


exception DoSkip
exception DontSkip

let is_good_meth ~classname (res,methname,args) = 
  try
    if skipMeth ~classname methname then raise DoSkip; 
    let lst = List.map ~f:fst args in
    let lst = res::lst in
    let _ = lst |> List.map ~f:(fun t -> t.t_name)  |> List.find ~f:skipArgument  in
    false

  with DoSkip -> true
    | DontSkip | Not_found -> false

exception BreakS of string

type t1 = string and t2 = string 
and castResult = Success of t1 | CastError of t2 | CastValueType of t2 | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult

type pattern = InvalidPattern | PrimitivePattern | ObjectPattern | EnumPattern

class virtual abstractGenerator _index = object (self)
    
  method private virtual prefix : string  
  method private virtual gen_class : string -> clas -> string option
  method private virtual gen_enumOfNs : string -> enum -> string option
  method private virtual gen_enumOfClass : string -> out_channel -> enum -> unit
  method private virtual genSlot  : string -> out_channel -> slt -> unit
  method private virtual genSignal : string -> out_channel -> sgnl -> unit
  method private virtual genProp : string -> out_channel -> prop -> unit
  method private virtual genMeth : string -> out_channel -> meth -> unit
  method private virtual genConstr : string -> out_channel -> constr -> unit
  method private virtual makefile : string -> string list -> unit

  method private index : index_data SuperIndex.t = _index

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
	       let key = NameKey.key_of_fullname name in
	       if is_enum_exn ~key self#index then EnumPattern
	       else if is_class_exn ~key self#index then InvalidPattern
	       else ( print_endline "you cant see this line. some fucking shit";
		      assert false )
	with
	    Not_found  -> (print_endline ("!!! Not in index: " ^ name);
				InvalidPattern )


  method private fromCamlCast 
    : index_data SuperIndex.t -> cpptype -> ?default:string option -> string -> castResult
      = fun index t ?(default=None) (argname(*arg name*):string) -> 
	let _ = default in
	let tname = t.t_name in
	let is_const = t.t_is_const in
	match self#pattern t with
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
	      | _ -> assert false)

	  | ObjectPattern -> Success (String.concat 
					[if is_const then "const " else ""; tname; "* _"; argname; 
					 " = (";tname;"*)"; argname; ";"])

	  | EnumPattern -> CastError ("cant't cast enum: " ^ (string_of_type t)) 
	      

  method private toCamlCast 
      = fun t arg ansVarName -> 
	match self#pattern t with
	  | InvalidPattern -> CastError ("Cant cast: " ^ (string_of_type t))
	  | PrimitivePattern ->
	    (match t.t_name with
	      | "int" -> Success (String.concat [ansVarName;" = Val_int("; arg; ");"])
	      | "double" -> CastError "Store_double_value!"
	      | "bool" -> Success (String.concat [ansVarName; " = Val_bool("; arg; ");"])
	      | "QString" -> Success (String.concat [ansVarName; " = caml_copy_string("; arg;
							".toLocal8Bit().data() );"])
	      | _ -> assert false)
	  | ObjectPattern -> Success (ansVarName ^ " = (value)(" ^ arg  ^ ");")
	  | EnumPattern   -> CastError ("cant't cast enum: " ^ (string_of_type t)) 
	
  method gen_ns dir ns =     
    if not (skip_ns ns.ns_name) then begin 
      let iter = List.iter in
      let dir  = dir ^ ns.ns_name in
      if Sys.file_exists dir then
	ignore (Sys.command ("rm -rf " ^ dir) );
      ignore (Sys.command ("mkdir -p -m 755 " ^ dir));
(*      Unix.mkdir dir 0o755; (* line above is recursive mkdir *) *)
      let classes = ref [] in (* classes and enums which was successfully generated *)
      let wrap = function
	| Some name -> classes := name :: !classes | None -> ()
      in
      iter ~f:(self#gen_ns self#prefix) ns.ns_ns;
      iter ~f:(fun c -> self#gen_class dir c |> wrap) ns.ns_classes;
      iter ~f:(fun e -> self#gen_enumOfNs dir e |> wrap) ns.ns_enums;
      self#makefile dir !classes;
    end

  method generate root_ns = self#gen_ns self#prefix root_ns
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
