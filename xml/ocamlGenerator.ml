open Parser
open Generators
open Printf

module V = struct type t = string end
module E = struct
  type t = int
  let compare = compare
  let default = 0
end
module G1 = Graph.Imperative.Digraph.AbstractLabeled(V)(E)
module G = struct 
  include G1
  let graph_attributes (_:t) = []
  let default_vertex_attributes _ = []
  let vertex_name v = 
    let name = G1.V.label v in
    str_replace [("&lt;","<");("&gt;",">");("<","_"); (">","_")] name
  let vertex_attributes v = [`Label (G1.V.label v)]
  let get_subgraph _ = None
  let default_edge_attributes _ = []
  let edge_attributes _ = []
end
module GraphPrinter = Graph.Graphviz.Dot (G)

exception BreakSilent
class ocamlGenerator dir (index:indexItem Index.t) =
  let iter = List.iter in
  let fprintf = Printf.fprintf in
  object (self)
  inherit abstractGenerator index as super

  method private prefix = dir ^ "/ml/"
  val  mutable graph = G.create ()
  method private ocamlClassName classname = 
    if classname.[0] = 'Q' then
      let s = String.copy classname in
      s.[0] <- 'q';
      s
    else
      classname

  method private genMethStubs classname h (meth_name, lst, res, acce, modlst) = 
    let meth_name = if (startswith ~prefix:(classname^"::") meth_name) then
	Str.string_after meth_name (String.length classname+2) else meth_name in
    try
      if goodMeth ~classname:classname meth_name res lst acce modlst then 
	raise BreakSilent;
      
      if List.length lst + 1 > 10 then
	raise BreakSilent;

      fprintf h "(* method %s `%s`(%s) *)\n"
	(self#type2str res) meth_name
	(List.map (self#type2str $ fst) lst |> String.concat ",");
      let cpp_func_name = self#cppFuncName classname meth_name lst in

      let lst2 = lst in
      let lst2 = List.map fst lst2 @ [res] in
      let types = List.map self#toOcamlType lst2 in

      let types = List.map (function
	| Success s -> s
	| _ -> raise BreakSilent
      ) types
      in
      fprintf h "external %s' : 'a -> %s\n\t\t= \"%s\"\n" meth_name 
	(types |> String.concat " -> ") cpp_func_name

    with BreakSilent -> () 
      |  BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  method toOcamlType t = match self#pattern t with
    | InvalidPattern -> CastError ("Cant cast: " ^ (self#type2str t) )
    | PrimitivePattern ->
      (match t.t_name with
	| "int" -> Success "int"
	| "double" -> CastError "double value!"
	| "bool" -> Success "bool"
	| "QString" -> Success "string"
	| "void" -> Success "unit"
	| _ -> assert false)

    | ObjectPattern -> Success "[> `qobject] obj"
    | EnumPattern -> CastError ("cant't cast enum: " ^ (self#type2str t)) 

  (* generates member code*)
  method genMeth classname h (meth_name,lst,res,acce,modlst) = 
    let meth_name = if (startswith ~prefix:(classname^"::") meth_name) then
	Str.string_after meth_name (String.length classname+2) else meth_name in
    try
      let skipStr = ("skipped " ^ classname ^ "::" ^ meth_name) in
      if goodMeth ~classname:classname meth_name res lst acce modlst then 
	raise (BreakS skipStr);
      
      if List.length lst + 1 > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      fprintf h "(* method %s `%s`(%s) *)\n"
	(self#type2str res) meth_name
	(List.map (self#type2str $ fst) lst |> String.concat ",");

      let lst2 = ({t_name=classname; t_is_ref=false; t_is_const=false; t_indirections=1;t_params=[] },None)
	:: lst in
      let lst2 = List.map fst lst2 @ [res] in
      let types = List.map self#toOcamlType lst2 in
(*      let cpp_func_name = self#cppFuncName classname meth_name lst in *)

      let types = List.map (function
	| Success s -> s
	| CastValueType s ->  raise (BreakS ("Casting value-type: " ^ s))
	|  (CastError s) -> raise (BreakS s)
	|  (CastTemplate str) -> raise (BreakS ("Casting template: "^ str))
      ) types
      in
      fprintf h "  method %s : %s\n\t\t= %s' me\n" meth_name 
	(types |> String.concat " -> ") meth_name

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  method genConstr classname h (lst,policy,modif) = ()

  method makefile dir lst = 
    let hh = open_out (dir^"/makefile.dot") in
    GraphPrinter.output_graph hh graph;
    close_out hh;
    let lst = List.fast_sort String.compare lst in
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "OCAMLC=ocamlc -g\nOCAMLOPT=ocamlopt -g\n\n";
    fprintf h "ML_MODULES=%s\n\n" (List.map (fun s -> s ^ ".cmo") lst |> String.concat " ");
    fprintf h ".SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx\n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "lablqt: $(ML_MODULES)\n\n";
(*    fptintf h ".PHONY: all"; *)
    close_out h

  method genClass dir c =       
    if skipClass c then None
    else begin
      let classname = fixTemplateClassName c.c_name in

      let cur = G.V.create classname in
      let addV name = 
	let v = G.V.create name in
	if not (G.mem_vertex graph v) then
	  G.add_vertex graph v;
	v
      in 
      let _ = addV classname in
      List.iter (fun name ->
	let v = addV name in
	G.add_edge graph cur v (* edge:  current -> base *)
      ) c.c_inherits;

      let h = open_out (dir ^ "/" ^ classname ^ ".ml") in
      iter (fun s -> fprintf h "open %s\n" s) c.c_inherits;
      fprintf h "\n";
      iter (self#genMethStubs classname h) c.c_meths;
      (*let classname = self#ocamlClassName classname in *)
      fprintf h "\nclass %s me = object (self) \n\n" classname;
       
      iter (self#genProp classname h) c.c_props;
      iter (self#genSignal classname h) c.c_sigs;
      iter (self#genSlot classname h) c.c_slots;
      iter (self#genMeth classname h) c.c_meths;
      iter (self#genEnumOfClass classname h) c.c_enums;
      iter (self#genConstr classname h) c.c_constrs;
      fprintf h "\nend\n";
      close_out h;
      Some classname
    end

  method genEnumOfNs _ (_,_) = None
  method genEnumOfClass classname h (name,lst) = 
    fprintf h "(* enum %s *)\n" name 
  method genSlot classname h (name,lst) = 
    fprintf h "(* slot %s(%s) *)\n" name (List.map (self#type2str $ fst ) lst |> String.concat ",")
  method genSignal classname h (name,lst) = 
    fprintf h "(* signal %s(%s) *)\n" name (List.map (self#type2str $ fst) lst |> String.concat ",")
  method genProp classname h (name,r,w) = 
    fprintf h "(* property %s " name;
    (match r with
      | Some s -> fprintf h "READ %s " s
      | None -> ());
    (match w with
      | Some s -> fprintf h "WRITE %s " s
      | None -> ());
    fprintf h " *)\n"

  method generate ns =
    super#generate ns;
    (* do something with graph *)
    ()

  method genNs dir ns =
    graph <- G.create ();
    super#genNs dir ns
end

