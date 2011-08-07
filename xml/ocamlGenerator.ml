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
  | s -> s

exception BreakSilent
class ocamlGenerator dir (index,g) = object (self)
  inherit abstractGenerator index as super

  method private prefix = dir ^/ "ml"
  val  mutable graph = g

  method toOcamlType t = match self#pattern t with
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
    try      
      if not (is_good_meth ~classname meth) then raise BreakSilent;      
      if List.length lst + 1 > 10 then raise BreakSilent;

      fprintf h "(* method %s *)\n" (string_of_meth meth);
      let cpp_func_name = cpp_func_name ~classname ~methname lst in

      let lst2 = List.map ~f:fst lst @ [res] in
      let types = List.map ~f:(self#toOcamlType) lst2 in

      let types = List.map ~f:(function
	| Success s -> s
	| _ -> raise BreakSilent
      ) types
      in
      fprintf h "external %s': 'a->%s\n\t\t= \"%s\"\n" methname 
	(String.concat types ~sep:"->") cpp_func_name

    with BreakSilent -> () 
      |  BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  (* generates member code*)
  method genMeth classname h meth = 
    let (res,methname,lst) = meth in
(*    let methname = ocaml_methname ~methname in *)
    (* We suppose that methname will be correct in OCaml *)
    try
      if not (is_good_meth ~classname meth) then 
	breaks 
	  (sprintf "skipped in class %s method %s --- not is_good_meth" classname (string_of_meth meth));
      if List.length lst + 1 > 10 then 
	breaks (sprintf "skipped meth %s: too many arguments" (string_of_meth meth) );

      fprintf h "(* method %s *)\n" (string_of_meth meth);

      let argnames = List.mapi lst ~f:(fun i _ -> "x"^(string_of_int i)) in
      let lst = List.map lst ~f:fst in
      let lst =(* {t_name=classname; t_is_ref=false; t_is_const=false; t_indirections=1;t_params=[] }
	::*) lst @ [res] in
      let types = List.map lst ~f:(self#toOcamlType) in

      let types = List.map types ~f:(function
	| Success s -> s
	| CastValueType s -> raise (BreakS ("Casting value-type: " ^ s) )
	| CastError s -> raise (BreakS s)
	| CastTemplate str -> raise (BreakS ("Casting template: "^ str))
      ) 
      in
      fprintf h "  method %s : %s\n\t\t= %s' me\n" methname 
	(String.concat types ~sep:"->") methname

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  method genConstr classname h (lst) = () 

  method makefile dir lst = 
(*    let hh = open_out (dir^"/makefile.dot") in
    GraphPrinter.output_graph hh graph;
    close_out hh;*)
    ignore (Sys.command ("touch " ^ dir ^/ ".depend"));
    let lst = List.fast_sort String.compare lst in
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "ML_MODULES=%s\n\n" (List.map lst ~f:(fun s -> s ^ ".cmx") |> String.concat ~sep:" ");
    fprintf h "OCAMLC=ocamlc -g\nOCAMLOPT=ocamlopt -g\n\n";
    fprintf h "INC=-I ./../../../test_gen\n";
    fprintf h ".SUFFIXES: .ml .mli .cmi .cmx\n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) $(INC) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC) -c $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "depend:\n\tocamldep $(INC) *.ml *.mli > .depend\n\n";
    fprintf h "lablqt: $(ML_MODULES)\n\n";
    fprintf h ".PHONY: all\n";
    fprintf h "include .depend";
    close_out h

  method gen_class ~prefix ~dir c = 
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then begin
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key);
      None
    end else if skipClass c then None
    else begin
      let classname = c.c_name in
      let h = open_out (dir ^/ classname ^ ".ml") in
      fprintf h "open Qtstubs\n\n";
      List.iter ~f:(fun s -> fprintf h "open %s\n" s) c.c_inherits;
      fprintf h "\n";

      if self#is_abstract_class ~prefix classname then
	fprintf h "(* class has pure virtual members - no constructors *)\n"
      else begin
	List.iter ~f:(self#gen_constr_stubs ~classname h) c.c_constrs; 
	List.iter ~f:(self#gen_constr       ~classname h) c.c_constrs
      end;

      let module S = String.Set in
      (* we chould generate different ocaml names for methods overloaded in C++ *)
      let meth_names = ref S.empty in      
      (* fold because no `map` =) *)
      let target_meths = MethSet.fold ~init:MethSet.empty c.c_meths_normal ~f:(fun m acc ->
	let (res,name,lst) = m in
	let name = ocaml_methname name in
	let new_name = 
	  if S.mem !meth_names name then name^"_"
	  else name in
	meth_names := S.add !meth_names new_name;
	MethSet.add acc (res,new_name,lst)	    
      ) in
      MethSet.iter target_meths ~f:(self#gen_meth_stubs ~classname h);

      let ocaml_classname = ocaml_class_name classname in
      fprintf h "\nclass %s me = object (self) \n" ocaml_classname;
      fprintf h "  method handler : [ `qobject ] obj = me\n";
      MethSet.iter target_meths ~f:(self#genMeth classname h);
      
(*      List.iter ~f:(self#genProp classname h) c.c_props;
      iter (self#genSignal classname h) c.c_sigs;
      iter (self#genSlot classname h) c.c_slots;
      iter (self#genEnumOfClass classname h) c.c_enums; *)
      fprintf h "\nend\n";
      close_out h;
      Some classname
    end
  method gen_constr_stubs ~classname h c = ()
  method gen_constr ~classname h c = ()
  method gen_enumOfNs ~prefix ~dir (_,_) = None
  method gen_enumOfClass classname h (name,lst) = 
    fprintf h "(* enum %s *)\n" name 
  method genSlot classname h (name,lst) = 
    fprintf h "(* slot %s(%s) *)\n" name (List.map ~f:(string_of_type $ fst ) lst |> String.concat ~sep:",")
  method genSignal classname h (name,lst) = 
    fprintf h "(* signal %s(%s) *)\n" name (List.map ~f:(string_of_type $ fst) lst |> String.concat ~sep:",")
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
    super#gen_ns ~prefix:[] ~dir ns
end

