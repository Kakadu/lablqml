open Parser 

let skipMeth name = 
  if startswith ~prefix:"operator" name then true
  else match name with
  | "d_func" -> true
  | _ -> false
let skipNs = function
  | "std" | "QGL" | "internal" | "QtConcurrent" | "QtPrivate" | "QDrawBorderPixmap" 
  | "QtSharedPointer" | "QMdi" | "QAlgorithmsPrivate" | "QAccessible2" -> true
  | _ -> false

let isTemplateClass name = 
    try ignore (String.index name '<'); true
    with Not_found -> false

let skipArgument  = function
  | "qreal" -> false
  | s when isTemplateClass s -> false
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

class virtual abstractGenerator = object (self)
    
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

  method type2str (_:cpptype) = ""

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
