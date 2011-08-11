open Core
open Core.Common
module List = Core_list
module String = Core_string
module Map = Core_map
open Parser
open Generators
open Printf
open SuperIndex

let ocaml_class_name classname = 
  if classname.[0] = 'Q' then
    let s = String.copy classname in
    s.[0] <- 'q';
    s
  else
    classname

let ocaml_methname ~methname = match methname with
  | "done" -> "done_1"
  | "open" -> "open_1"
  | "type" -> "type_1"
  | "begin" -> "begin_1"
  | "end"  -> "end_1"
  | "let" -> "let_1"
  | "object" -> "object_1"
  | "struct" -> "struct_1"
  | "module" -> "module_1"
  | s -> s


exception BreakSilent
class ocamlGenerator dir ((index,g):index_t*G.t) = object (self)
(*  inherit abstractGenerator index as super *)
  method private prefix = dir ^/ "ml"
(*  method pattern _ = InvalidPattern *)
  val  mutable graph = g
  method toOcamlType t = match pattern index t with
    | InvalidPattern -> CastError (sprintf "Cant cast: %s" (string_of_type t) )
    | PrimitivePattern ->
      (match t.t_name with
	| "int" -> Success "int"
	| "double" -> CastError "double value!"
	| "bool" -> Success "bool"
	| "QString" -> Success "string"
	| "void" -> Success "unit"
	| "char" when t.t_indirections=1 -> Success "string"
	| _ -> assert false)
    | ObjectPattern -> Success "[> `qobject] obj" 
    | EnumPattern -> CastError (sprintf "cant't cast enum: %s" (string_of_type t) )


  method private gen_meth_stubs ~classname h meth = 
    let (res,methname,lst) = meth in
    let methname = ocaml_methname ~methname in
    let ocaml_classname = ocaml_class_name classname in
    try      
      if not (is_good_meth ~classname meth) then raise BreakSilent;      
      if List.length lst + 1 > 10 then raise BreakSilent;
      if List.length lst + 1 > 5 then raise BreakSilent; (* need additional stub for native compilation *)

      fprintf h "(* method %s *)\n" (string_of_meth meth);
      let cpp_func_name = cpp_func_name ~classname ~methname lst in

      let lst2 = List.map ~f:fst lst @ [res] in
      let types = List.map ~f:(self#toOcamlType) lst2 in

      let types = List.map ~f:(function
	| Success s -> s
	| _ -> raise BreakSilent
      ) types
      in
      fprintf h "external %s_%s': 'a->%s\n\t\t= \"%s\"\n" ocaml_classname methname 
	(String.concat types ~sep:"->") cpp_func_name

    with BreakSilent -> () 
      |  BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  (* generates member code*)
  method gen_meth classname h meth = 
    let (res,methname,lst) = meth in
    let methname = ocaml_methname methname in
    let ocaml_classname = ocaml_class_name classname in
    try
      if not (is_good_meth ~classname meth) then 
	breaks 
	  (sprintf "skipped in class %s method %s --- not is_good_meth" classname (string_of_meth meth));

      if List.length lst + 1 > 10 then 
	breaks (sprintf "skipped meth %s: too many arguments" (string_of_meth meth) );

      if List.length lst  +1 > 5 then
	breaks (sprintf "needs additional func for native");

      fprintf h "(* method %s *)\n" (string_of_meth meth);
      (match self#toOcamlType res with
	| Success _ -> ()
	| _ -> breaks "cant cast result type");

      let lst = List.map lst ~f:fst in
      let types = List.map lst ~f:(self#toOcamlType) in
      let argnames = List.mapi lst ~f:(fun i _ -> "x"^(string_of_int i)) in

      let types: string list = List.map types ~f:(function
	| Success s -> s
	| CastValueType s -> raise (BreakS ("Casting value-type: " ^ s) )
	| CastError s -> raise (BreakS s)
	| CastTemplate str -> raise (BreakS ("Casting template: "^ str))
      ) 
      in
      let args2 = List.map3 types lst argnames ~f:(fun ttt start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern  -> name
	  | ObjectPattern -> "(" ^ name ^ "#handler)" ) in
      let args1 = List.map3 types lst argnames ~f:(fun ttt start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern -> name
	  | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name) 
      ) in
	
      fprintf h "  method %s %s\n\t\t= %s_%s' me %s %s\n"  methname 
	(String.concat ~sep:" " args1)
	ocaml_classname methname 
	(String.concat ~sep:" " args2) (match pattern index res with
	  | ObjectPattern -> (" |> new " ^ (ocaml_class_name res.t_name))
	  | _ -> "")

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

(*  method genConstr classname h (lst) = () *)

  method makefile dir = 
    ignore (Sys.command ("touch " ^ dir ^/ ".depend"));
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "ML_MODULES=stubs.cmx classes.cmx\n\n";
    fprintf h "OCAMLC=ocamlc -g\nOCAMLOPT=ocamlopt -g\n\n";
    fprintf h "INC=-I ./../../test_gen\n";
    fprintf h ".SUFFIXES: .ml .mli .cmi .cmx\n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) $(INC) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "depend:\n\tocamldep $(INC) *.ml *.mli > .depend\n\n";
    fprintf h "lablqt: $(ML_MODULES)\n\n";
    fprintf h ".PHONY: all clean\n";
    fprintf h "include .depend\n";
    fprintf h "clean:\n\trm -f *.cmi *.cmx *.cmo *.o\n\n";
    close_out h
    
  method gen_class ~prefix h_classes h_stubs c = 
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then begin
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key)      
    end else if skipClass c then ()
    else begin
      let classname = c.c_name in
      let is_abstract = is_abstract_class ~prefix index classname in
      fprintf h_stubs "(* ********** class %s *********** *)\n" classname;

      if is_abstract then
	fprintf h_stubs "(* class has pure virtual members - no constructors *)\n"
      else 
	List.iter ~f:(self#gen_constr_stubs ~classname h_stubs) c.c_constrs
      ;
      
      let module S = String.Set in
      (* we chould generate different ocaml names for methods overloaded in C++ *)
      let meth_names = ref S.empty in      
      (* fold because no `map` =) *)
      let target_meths = MethSet.fold ~init:MethSet.empty c.c_meths_normal ~f:(fun m acc ->
	let (res,name,lst) = m in
	let name = ocaml_methname name in
	let rec loop name = if S.mem !meth_names name then loop (name^"_") else name in

	let new_name = loop name in
	meth_names := S.add !meth_names new_name;
	MethSet.add acc (res,new_name,lst)	    
      ) in
      MethSet.iter target_meths ~f:(self#gen_meth_stubs ~classname h_stubs); 

      List.iter ~f:(self#gen_constr       ~classname h_classes) c.c_constrs;
      let ocaml_classname = ocaml_class_name classname in
      fprintf h_classes " %s me = object (self) \n" ocaml_classname;
      fprintf h_classes "  method handler : [ `qobject ] obj = me\n";
      MethSet.iter target_meths ~f:(self#gen_meth classname h_classes);
      fprintf h_classes "end\nand ";
    end

  method gen_constr_stubs ~classname h c = ()
  method gen_constr ~classname h c = ()
(*  method gen_enumOfNs ~prefix ~dir (_,_) = None
  method gen_enumOfClass classname h (name,lst) = ()
  method genSlot classname h (name,lst) = ()
  method genSignal classname h (name,lst) =  ()
  method genProp classname h (name,r,w) = () *)
  
  method generate ns =
    self#makefile dir;
    let h1 = open_out (dir ^/ "classes.ml") in
    let h2 = open_out (dir ^/ "stubs.ml") in
    fprintf h1 "\nopen Stubs\nopen Qtstubs\n\nclass ";
    fprintf h2 "open Qtstubs\n\n";

    List.iter ns.ns_classes ~f:(self#gen_class ~prefix:[] h1 h2);
    fprintf h1 "aa = object end";
    close_out h2;
    close_out h1;
    ()
end

