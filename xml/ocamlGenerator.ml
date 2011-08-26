open Core
open Core.Common
module List = Core_list
module String = Core_string
module Map = Core_map
module Q = Core_queue
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

exception Break2File of string 
let break2file s = raise (Break2File s)

class ocamlGenerator dir (index:index_t) = object (self)
  method private prefix = dir ^/ "ml"
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

  method gen_constr ~classname ~i h argslist = 
    (* TODO: maybe diable copy constructors *)
    try
      if List.length argslist > 5 then raise BreakSilent;
      fprintf h "\n(* constructor %s *)\n" (string_of_constr ~classname argslist);
      let argnames = List.mapi argslist ~f:(fun i _ -> "x"^(string_of_int i)) in
      let cpp_func_name = cpp_func_name ~classname ~methname:classname argslist in
      let lst = List.map argslist ~f:fst in
      let types = List.map lst ~f:(self#toOcamlType) in
      let types = List.map types ~f:(function | Success s -> s | _ -> raise BreakSilent) in

      fprintf h "external create_%s_%d' : %s -> [>`qobject] obj = \"%s\"\n" classname i
	(if List.length types = 0 then "unit" else String.concat ~sep:"->" types) 
	cpp_func_name;

      let args2 = List.map3_exn types lst argnames ~f:(fun _ start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern  -> name
	  | ObjectPattern -> "(" ^ name ^ "#handler)" ) in
      let args1 = List.map3_exn types lst argnames ~f:(fun ttt start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern -> sprintf "(%s: %s)" name ttt
	  | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name) 
      ) in
      fprintf h "let create_%s_%d %s = create_%s_%d' %s\n" classname i 
	(String.concat ~sep:" " args1)
	classname i
	(String.concat ~sep:" " args2)
      
    with BreakSilent -> ()

  method private gen_meth_stubs ~is_abstract ~classname h meth =
    try   
      (match meth.m_access with `Public -> () | `Private | `Protected -> raise BreakSilent);

      if not (is_good_meth ~classname meth) then 
	breaks (sprintf "not is_good_meth %s" (string_of_meth meth) );
      if List.length meth.m_args + 1 > 10 then raise BreakSilent;
      (* need additional stub for native compilation *)
      if List.length meth.m_args + 1 > 5 then raise BreakSilent;

      let ocaml_classname = ocaml_class_name classname in

      fprintf h "(* method %s *)\n" (string_of_meth meth);
      let cpp_func_name = cpp_func_name ~classname:meth.m_declared ~methname:meth.m_name meth.m_args in

      let lst2 = List.map ~f:fst meth.m_args @ [meth.m_res] in
      let types = List.map ~f:(self#toOcamlType) lst2 in

      let types = List.map types ~f:(function
	| Success s -> s
	| _ -> raise BreakSilent
      ) in
      fprintf h "external %s_%s': 'a->%s\n\t\t= \"%s\"\n" ocaml_classname meth.m_out_name 
	(String.concat types ~sep:"->") cpp_func_name

    with BreakSilent -> () 
      |  BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )

  (* generates member code*)
  method gen_meth ~is_abstract ~classname h meth = 
    let (res,methname,lst) = (meth.m_res,meth.m_name,meth.m_args) in
    try
      (match meth.m_access with `Public -> () | `Private | `Protected -> raise BreakSilent);
      if not (is_good_meth ~classname meth) then 
	break2file
	  (sprintf "skipped in class %s method %s --- not is_good_meth" classname (string_of_meth meth));

      if List.length lst + 1 > 10 then 
	break2file (sprintf "skipped meth %s: too many arguments\n" (string_of_meth meth) );

      if List.length lst  +1 > 5 then
	break2file (sprintf "needs additional func for native\n");

      fprintf h "  (* method %s *)\n" (string_of_meth meth);
      let res_type = match self#toOcamlType res with
	| Success s -> s
	| _ -> break2file "cant cast result type" in

      let lst = List.map lst ~f:fst in
      let types = List.map lst ~f:(self#toOcamlType) in
      let argnames = List.mapi lst ~f:(fun i _ -> "x"^(string_of_int i)) in

      let types = List.map types ~f:(function
	| Success s -> s
	| CastValueType s -> breaks ("Casting value-type: " ^ s) 
	| CastError s -> breaks s
	| CastTemplate str -> raise (BreakS ("Casting template: "^ str))
      ) 
      in
      let args2 = List.map3_exn types lst argnames ~f:(fun _ start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern  -> name
	  | ObjectPattern -> "(" ^ name ^ "#handler)" ) in
      let args1 = List.map3_exn types lst argnames ~f:(fun ttt start name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern -> sprintf "(%s: %s)" name ttt
	  | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in
	
      if is_abstract then begin
	fprintf h "  method virtual %s : %s" meth.m_out_name
	  (String.concat ~sep:"->" types );
	if List.length types <> 0 then fprintf h " -> ";
	match pattern index res with
	  | ObjectPattern -> fprintf h "%s" (ocaml_class_name res.t_name)
	  | _ ->      fprintf h "%s" res_type
      end else begin
	fprintf h "  method %s %s = %s_%s' me %s " meth.m_out_name (String.concat ~sep:" " args1)
	  (ocaml_class_name meth.m_declared) meth.m_out_name (String.concat ~sep:" " args2);
	(match pattern index res with
	  | ObjectPattern -> fprintf h "|> new %s" (ocaml_class_name res.t_name)
	  | _ -> ());	
      end;
      fprintf h "\n";

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )
      | BreakSilent -> ()
      | Break2File s -> ( fprintf h "(* %s *)\n" s; print_endline s)

  method makefile dir = 
    ignore (Sys.command ("touch " ^ dir ^/ ".depend"));
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "ML_MODULES=stubs.cmo classes.cmo creators.cmo \n\n";
    fprintf h "OCAMLC=ocamlc -g\nOCAMLOPT=ocamlopt -g\n\n";
    fprintf h "INC=-I ./../../test_gen \n";
    fprintf h ".SUFFIXES: .ml .mli .cmi .cmx .cmo \n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC)   $(INC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) $(INC) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC)  $(INC) -c $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "depend:\n\tocamldep $(INC) *.ml *.mli > .depend\n\n";
    fprintf h "lablqt: $(ML_MODULES)\n\n";
    fprintf h ".PHONY: all clean\n";
    fprintf h "include .depend\n";
    fprintf h "clean:\n\trm -f *.cmi *.cmx *.cmo *.o\n\n";
    close_out h
    
  method gen_class ~prefix h_classes h_stubs h_constrs c = 
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then 
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key)      
    else if skipClass ~prefix c then print_endline ("Skipping class " ^ c.c_name)
    else begin
      let classname = c.c_name in
      printf "Generating class %s\n" classname;
      let is_abstract = is_abstract_class ~prefix index classname in
      let ocaml_classname = ocaml_class_name classname in
      
      fprintf h_stubs "\n(* ********** class %s *********** *)\n" classname;
      if is_abstract then
	fprintf h_constrs "(* class %s has pure virtual members - no constructors *)\n" classname
      else (
	List.iteri c.c_constrs ~f:(fun i c ->
	  self#gen_constr ~classname ~i h_constrs c	  
	)
      );
      
      let target_meths = c.c_meths in
      fprintf h_classes " %s me = object (self) \n" 
(*        (if is_abstract then "virtual " else "") *) ocaml_classname;
      fprintf h_classes "  method handler : [ `qobject ] obj = me\n";
      List.iter c.c_sigs  ~f:(self#gen_signal h_classes); 
      MethSet.iter c.c_slots ~f:(fun slot -> 
	self#gen_slot ~classname h_classes slot;
	self#gen_meth_stubs ~is_abstract ~classname h_stubs slot
      );

      MethSet.iter target_meths ~f:(fun m -> 
	self#gen_meth_stubs ~is_abstract ~classname h_stubs m;
	self#gen_meth       ~is_abstract:false ~classname h_classes m 
      ); 
      fprintf h_classes "end\nand ";
    end

  method gen_slot : classname:string -> out_channel -> Parser.slt -> unit = 
    fun ~classname h slot ->
    try
      (match slot.m_access with `Public -> () | `Private | `Protected -> raise BreakSilent);
      let (name,args,modif) = (slot.m_name,slot.m_args, slot.m_access) in
      let types = List.map args ~f:(self#toOcamlType $ fst) in
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let types = List.map2_exn args types ~f:(fun (arg,_) sometype ->
	match pattern index arg with
	  | ObjectPattern -> ocaml_class_name arg.t_name
	  | _ -> sometype) in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      fprintf h "  method slot_%s = object (self : (< %s .. >, %s unit) #ssslot)\n"
	slot.m_out_name 
	(List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ")
	(if List.is_empty types then "" else (String.concat ~sep:" -> " types) ^ " -> ");
	
      fprintf h "    method name = \"%s\"\n" name;
(*      fprintf h "    method call = %s_%s' me\n" (ocaml_class_name classname) slot.slt_out_name; *)
      let argnames = List.mapi args ~f:(fun i _ -> sprintf "x%d" i) in
      let argnames2 = List.map3_exn args types argnames ~f:(fun (start,_) ttt name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern -> sprintf "(%s: %s)" name ttt
	  | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in
      let argnames3 = List.map3_exn args types argnames ~f:(fun (start,_) ttt name ->
	match pattern index start with
	  | EnumPattern | InvalidPattern -> assert false
	  | PrimitivePattern -> sprintf "%s" name
	  | ObjectPattern -> sprintf "%s#handler" name
      ) in
      
      fprintf h "    method call %s = %s_%s' me %s |> ignore \n" 
	(String.concat ~sep:" " argnames2) 
	(ocaml_class_name classname) slot.m_out_name
	(String.concat ~sep:" " argnames3);
      fprintf h "  end\n"
    with BreakSilent -> ()
      | BreakS s -> ()

  method gen_signal: out_channel -> Parser.sgnl -> unit = fun h (name,args) ->
    let args = List.map args ~f:fst in
    let types = List.map args ~f:(self#toOcamlType) in
    try
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      let types = List.map2_exn args types ~f:(fun arg sometype ->
	match pattern index arg with
	  | ObjectPattern -> ocaml_class_name arg.t_name
	  | _ -> sometype) in
      fprintf h "  method signal_%s = object (self : <%s ..> #sssignal)\n"
	name (List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ");
      fprintf h "    method name = \"%s\"\n" name;
      fprintf h "  end\n";
    with BreakSilent -> ()
      | BreakS s -> ()

(*  method gen_enumOfNs ~prefix ~dir (_,_) = None
  method gen_enumOfClass classname h (name,lst) = ()
  method genSlot classname h (name,lst) = ()
  method genSignal classname h (name,lst) =  ()
  method genProp classname h (name,r,w) = () *)
  
  method generate queue =
    self#makefile dir;
    let h1 = open_out (dir ^/ "classes.ml") in
    let h2 = open_out (dir ^/ "stubs.ml") in
    let h3 = open_out (dir ^/ "creators.ml") in
    fprintf h1 "\nopen Stubs\nopen Qtstubs\n\nclass";
    fprintf h2 "open Qtstubs\n\n";
    fprintf h3 "open Qtstubs\nopen Stubs\nopen Classes\n";

    printf "Queue length is %d\n" (Q.length queue);
    Q.iter queue ~f:(fun key-> 
      match SuperIndex.find_exn index key with
	| Enum  e -> ()
	| Class (c,_) -> self#gen_class ~prefix:[] h1 h2 h3 c 
    );

    fprintf h1 " aa = object end";
    close_out h3;
    close_out h2;
    close_out h1
    
end
