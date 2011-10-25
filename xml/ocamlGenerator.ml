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

exception Break2File of string 
let break2file s = raise (Break2File s)

class ocamlGenerator dir (index:index_t) = object (self)
  method private prefix = dir ^/ "ml"

  (* low_level means, for example, to generate [`qobject] obj instead of qObject *)
  method toOcamlType ?forcePattern ~low_level ( arg: Parser.func_arg) =
    let {arg_type=t; arg_default=default; _} = arg in
    let patt = match forcePattern with
      | None   -> pattern index arg 
      | Some x  -> x in
    match patt with
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
    | ObjectPattern ->
      Success (
	sprintf "%s" (if low_level then "[> `qobject] obj" else ocaml_class_name t.t_name)
      )
    | ObjectDefaultPattern -> 
      Success (
	sprintf "%s option" (if low_level then "[> `qobject] obj" else ocaml_class_name t.t_name)
      )
    | EnumPattern ({e_items;_},key) ->
      let s = String.concat ~sep:" | " (List.map e_items (fun x -> "`"^x)) in
      Success (sprintf "[%s]" s)

  method gen_constr ~classname ~i h argslist = 
    (* TODO: maybe disable copy constructors *)
    try
      if List.length argslist > 5 then break2file "more then 5 parameters";
      fprintf h "\n(* constructor %s *)\n" (string_of_constr ~classname argslist);
      let argnames = List.mapi argslist ~f:(fun i _ -> "x"^(string_of_int i)) in 

      let cpp_func_name classname = 
	let native_name = cpp_stub_name ~classname ?res_n_name:None ~is_byte:false `Public argslist in
	if List.length argslist > 5 then
	  sprintf "%s\" \"%s" (cpp_stub_name ~classname ?res_n_name:None ~is_byte:true `Public argslist)
	    native_name
	else
	  native_name
      in
      let cpp_twin_func_name = cpp_func_name (classname^"_twin") in
      let cpp_func_name  = cpp_func_name classname in

      let types = List.map argslist ~f:(fun arg -> 
(*	let patt = pattern index arg in
	printf "ocaml====pattern of %s is %s\n" (arg |> sexp_of_func_arg |> Sexplib.Sexp.to_string_hum) 
	  (patt |> sexp_of_pattern |> Sexplib.Sexp.to_string); 	*)
	self#toOcamlType ~low_level:true arg
      ) in
      let types = List.map types ~f:(function | Success s -> s | _ -> break2file "toOcamlType failed") in
      let no_args = (List.length types = 0) in

      let ocaml_func_name = sprintf "create_%s_%d" classname i in
      let ocaml_twin_func_name =  sprintf "create_%s_twin_%d" classname i in
      
      let print_stub caml_name stub_name =
	fprintf h "external %s' : %s -> [>`qobject] obj = \"%s\"\n" caml_name
	  (if no_args then "unit" else ( String.concat ~sep:"->" types) )
	  stub_name
      in
      print_stub ocaml_func_name cpp_func_name;
      print_stub ocaml_twin_func_name cpp_twin_func_name;

      let types = List.map argslist ~f:(self#toOcamlType ~low_level:false) in
      let types = List.map types ~f:(function | Success s -> s | _ -> break2file "toOcamlType failed") in

      let args2 = List.map3_exn types argslist argnames ~f:(fun _ arg name ->
	match pattern index arg with
	  | InvalidPattern -> assert false
	  | EnumPattern _
	  | PrimitivePattern -> name
	  | ObjectPattern    -> sprintf " (%s#handler) " name
	  | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" cpp_func_name name name
      ) in
      let args1 = List.map3_exn types argslist argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
	match pattern index arg with
	  | InvalidPattern -> assert false
	  | EnumPattern _
	  | PrimitivePattern -> sprintf "(%s: %s)" name ttt
	  | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
	  | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in
      let print_main_creator ocaml_func_name =
	let s1 = sprintf "let %s %s = " ocaml_func_name 
	  (if no_args then "()" else String.concat ~sep:" " args1) in
	fprintf h "%s" s1;
	if String.length s1 > 60 then fprintf h "\n    ";
	
	fprintf h "%s' %s %s\n" ocaml_func_name (if no_args then "()" else String.concat ~sep:" " args2)
	  (sprintf "\n    |> new %s" (ocaml_class_name classname))
      in
      print_main_creator ocaml_func_name;
      print_main_creator ocaml_twin_func_name;      
      ()
    with BreakSilent -> ()
      | Break2File s -> fprintf h "(* %s *)\n" s

  method private gen_meth_stubs ~is_abstract ~classname h meth =
      fprintf h "  (* method %s *)\n" (string_of_meth meth);

      let ocaml_classname = ocaml_class_name classname in

      let cpp_func_name = 
	(* Some hack. If this meth has < 5 parameters that string format is similiar to [a-z]+ 
	   But if it needs to functions  for stubs format is [a-z]+" "[a-z]+
	*)
	let classname = meth.m_declared in
	let res_n_name = (meth.m_res,meth.m_name) in
	let helper is_byte = cpp_stub_name ~classname ~res_n_name ~is_byte meth.m_access meth.m_args in
	let native_name = helper false in
	if List.length meth.m_args > 5 then
	  sprintf "%s\" \"%s" (helper true) native_name
	else
	  native_name
      in

      let types = List.map meth.m_args ~f:(fun arg -> self#toOcamlType ~low_level:true arg ) in
      let types = List.map types ~f:(function Success s -> s | _ -> assert false) in

      let res_arg = simple_arg meth.m_res in
      let res_t = (match pattern index res_arg with
	| ObjectPattern -> self#toOcamlType ~forcePattern:ObjectDefaultPattern ~low_level:true res_arg
	| ObjectDefaultPattern -> assert false
	| _ -> self#toOcamlType ~low_level:true res_arg)
      |> (function Success s -> s | _ -> raise BreakSilent) in

      fprintf h "external %s_%s': 'a->%s\n\t\t= \"%s\"\n" ocaml_classname meth.m_out_name 
	(String.concat (types @ [res_t]) ~sep:"->") cpp_func_name

  (* generates member code*)
  method gen_meth ?new_name ~is_abstract ~classname h meth = 
    let (res,methname,args) = (meth.m_res,meth.m_name,meth.m_args) in
    try
      let () = match meth.m_access with `Private -> raise BreakSilent | _ -> () in

      if List.length args + 1 > 10 then 
	break2file (sprintf "skipped meth %s: too many arguments\n" (string_of_meth meth) ); 

      fprintf h "  (* method %s *)\n" (string_of_meth meth);
      let res_type = 
	let res_arg = simple_arg res in
	(match pattern index res_arg with
	  | ObjectPattern -> self#toOcamlType ~forcePattern:ObjectDefaultPattern ~low_level:false res_arg 
	  | ObjectDefaultPattern -> assert false
	  | _ -> self#toOcamlType ~low_level:false res_arg)
        |> (function
	    | Success s -> s
	    | _ -> break2file "cant cast result type" )
      in
      let types = List.map args ~f:(fun arg -> self#toOcamlType ~low_level:false arg) in
      let argnames = List.mapi args ~f:(fun i _ -> "x"^(string_of_int i)) in

      let types = List.map types ~f:(function
	| Success s -> s
	| CastValueType s -> breaks ("Casting value-type: " ^ s) 
	| CastError s -> breaks s
	| CastTemplate str -> breaks ("Casting template: "^ str)
      ) 
      in
	
      if is_abstract then begin
	fprintf h "  method virtual %s : %s" meth.m_out_name (String.concat ~sep:"->" types );
	if List.length types <> 0 then fprintf h " -> ";
	match pattern index (simple_arg res) with
	  | ObjectPattern -> fprintf h "%s option" (ocaml_class_name res.t_name)
	  | _ -> fprintf h "%s" res_type
      end else begin
	let args2 = List.map3_exn types args argnames ~f:(fun _ arg name ->
	  match pattern index arg with
	    | InvalidPattern -> assert false
	    | EnumPattern _
	    | PrimitivePattern  -> name
	    | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" meth.m_out_name name name
	    | ObjectPattern -> sprintf "(%s#handler)" name
	) in
	let args1 = List.map3_exn types args argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
	  match pattern index arg with
	    | InvalidPattern -> assert false
	    | EnumPattern _
	    | PrimitivePattern -> sprintf "(%s: %s)" name ttt
	    | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
	    | ObjectPattern -> sprintf "(%s: %s )" name (ocaml_class_name start.t_name)
	) in
	let meth_name = match new_name with None -> meth.m_out_name | Some x -> x in
	fprintf h "  method %s %s = %s_%s' self#handler %s\n" meth_name (String.concat ~sep:" " args1)
	  (ocaml_class_name meth.m_declared) meth.m_out_name (String.concat ~sep:" " args2);
	(match pattern index (simple_arg res) with
	  | ObjectPattern -> 
	    let res_classname = ocaml_class_name res.t_name in
	    fprintf h "|> (function Some o -> Some ( match Qtstubs.get_caml_object o with\n";
	    fprintf h "                            | Some x -> (x:>%s)\n" res_classname;
	    fprintf h "                            | None -> new %s o)\n" res_classname;
	    fprintf h "   | None -> None)\n"

	  | _ -> ());	
      end;
      fprintf h "\n";

    with BreakS str -> ( fprintf h "(* %s *)\n" str;
			 print_endline str )
      | BreakSilent -> ()
      | Break2File s -> ( fprintf h "(* %s *)\n" s; print_endline s)

  method gen_slot = 
    fun ~classname h slot ->
    try
      (match slot.m_access with `Public -> () | `Private | `Protected -> raise BreakSilent);
      let (name,args) = (slot.m_name,slot.m_args) in
      let res = simple_arg slot.m_res in
      let types = List.map args ~f:(fun arg -> self#toOcamlType ~low_level:false arg) in
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let res_t = match self#toOcamlType ~low_level:false res with
	| Success s -> s | _ -> raise BreakSilent in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      fprintf h "  method slot_%s = object (self : (< %s .. >, %s ) #ssslot)\n"
	slot.m_out_name 
	(List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ")
	(String.concat ~sep:" -> " (types @ [res_t]));
      let wrap_type t = { { t with t_is_const=false } with t_is_ref=false } in
      fprintf h "    method name = \"%s(%s)\"\n" name 
	(slot.m_args |> List.map ~f:(fun x -> string_of_type (wrap_type x.arg_type) ) 
	    |> String.concat ~sep:",");
      self#gen_meth ~new_name:"call" ~is_abstract:false ~classname h slot;
      fprintf h "  end\n"
    with BreakSilent -> ()
      | BreakS s -> ()

  method gen_signal = fun h (name,args) ->
    try
      let types = List.map args ~f:(self#toOcamlType ~low_level:false) in
      let types = List.map types ~f:(function Success s -> s | _ -> raise BreakSilent) in
      let argnames = List.mapi args ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      fprintf h "  method signal_%s = object (self : <%s ..> #sssignal)\n"
	name (List.map2_exn argnames types ~f:(fun name t -> name^":"^t^";") |> String.concat ~sep:" ");
      let wrap_type t = {{ t with t_is_ref=false } with t_is_const = false } in
      fprintf h "    method name = \"%s(%s)\"\n" name
	(List.map args ~f:(fun x -> string_of_type (wrap_type x.arg_type) ) |> String.concat ~sep:",");
      fprintf h "  end\n";
    with BreakSilent -> ()
      | BreakS s -> ()

  method generate queue =
    self#makefile dir;
    let h1 = open_out (dir ^/ "classes.ml") in
    let h2 = open_out (dir ^/ "stubs.ml") in
    let h3 = open_out (dir ^/ "creators.ml") in
    fprintf h1 "\nopen Stubs\nopen Stub_helpers\nclass";
    fprintf h2 "open Stub_helpers\n\n";
    fprintf h3 "open Stub_helpers\nopen Classes\nopen Stubs\n";

    printf "Queue length is %d\n" (Q.length queue);
    Q.iter queue ~f:(fun key ->
      match SuperIndex.find_exn index key with
	| Enum  e -> ()
	| Class (c,_) -> self#gen_class ~prefix:[] h1 h2 h3 c 
    );

    fprintf h1 " aa = object end";
    close_out h3;
    close_out h2;
    close_out h1

  method private gen_constr_data ~classname ~index argslist =
(*    let arg_count = List.length constr in *)
    (* TODO: construct argnames using C++ argument names *)
    let argnames = List.mapi argslist ~f:(fun i _ -> "x"^(string_of_int i)) in
    let types = List.map argslist ~f:(fun arg -> self#toOcamlType ~low_level:false arg) in
    (* With_return  ???? *)
    let types = List.map types ~f:(function Success s -> s 
      | _ -> assert false (*because we've checked that all constructors are correct in Filter module *)) in
    (* paramters of create* function or class's parameters *)
    let args1 = List.map3_exn types argslist argnames ~f:(fun ttt ({arg_type=start;_} as arg) name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
          | PrimitivePattern -> sprintf "(%s: %s)" name ttt
          | ObjectDefaultPattern -> sprintf "(%s: %s option)" name (ocaml_class_name start.t_name)
          | ObjectPattern -> sprintf "(%s: %s)" name (ocaml_class_name start.t_name)
      ) in
    (* arguments for calling external function *)
    let args2 = List.map3_exn types argslist argnames ~f:(fun _ arg name ->
        match pattern index arg with
          | InvalidPattern -> assert false
          | EnumPattern _
	      (* We do low_level casts here because  we call low_level creator when class's initializer *)
          | PrimitivePattern -> name
          | ObjectPattern    -> sprintf " (%s#handler) " name
          | ObjectDefaultPattern -> sprintf "(wrap_handler \"%s\" \"%s\" %s)" "don't know" name name
      ) in    
    (String.concat ~sep:" " args1, String.concat ~sep:" " args2)

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
      
      if is_abstract then begin
	fprintf h_classes " virtual %s me = object(self)\n" ocaml_classname;
	fprintf h_classes " method handler : [`qobject] obj = me \n\n"
      end else begin
	let constr = List.hd_exn c.c_constrs in
(*	let lst1,lst2 = self#gen_constr_data  ~classname ~index constr in *)
	fprintf h_classes " %s me = object(self)\n" ocaml_classname;
	fprintf h_classes " method handler : [`qobject] obj = me\n";
	fprintf h_classes " initializer Qtstubs.set_caml_object me self\n"
(*	fprintf h_classes " method get_handler = handler\n\n"; 
	fprintf h_classes " initializer handler <- %s self %s\n\n" 
	  (sprintf "create_%s_0'" classname) lst2	*)
      end;

      List.iter c.c_sigs  ~f:(self#gen_signal h_classes);
(*      MethSet.iter c.c_slots ~f:(fun slot -> 
	if is_good_meth ~classname ~index slot then begin
	  (*self#gen_slot ~classname h_classes slot; *)
	  self#gen_meth_stubs ~is_abstract ~classname h_stubs slot
	end 
      ); *)
      MethSet.iter c.c_meths ~f:(fun m -> 
	match m.m_modif with
	  | `Normal when is_good_meth ~classname ~index m ->	    
	    self#gen_meth_stubs ~is_abstract ~classname h_stubs m;
	    self#gen_meth       ~is_abstract:false ~classname h_classes m 
	  | _ -> ()
      ); 

      fprintf h_classes "end\nand ";
    end

  method makefile dir = 
    ignore (Sys.command ("touch " ^ dir ^/ ".depend"));
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "ML_MODULES=stubs.cmo creators.cmo classes.cmo \n";
    fprintf h "ML_MODULES_OPT=stubs.cmx creators.cmx classes.cmx  \n\n";
    fprintf h "OCAMLC=ocamlc -g\nOCAMLOPT=ocamlopt -g\n\n";
    fprintf h "INC=-I ./../../test_gen \n";
    fprintf h ".SUFFIXES: .ml .mli .cmi .cmx .cmo \n\n";
    fprintf h ".ml.cmo:\n\t$(OCAMLC)   $(INC) -c $<\n\n";
    fprintf h ".ml.cmx:\n\t$(OCAMLOPT) $(INC) -c $<\n\n";
    fprintf h ".mli.cmi:\n\t$(OCAMLC)  $(INC) -c $<\n\n";
    fprintf h "all: byte opt\n\n";
    fprintf h "depend:\n\tocamldep $(INC) *.ml *.mli > .depend\n\n";
    fprintf h "byte: $(ML_MODULES)\n\n";
    fprintf h "opt:  $(ML_MODULES_OPT)\n\n";
    fprintf h ".PHONY: all clean opt byte\n\n";
    fprintf h "include .depend\n\n";
    fprintf h "clean:\n\trm -f *.cm[iox] *.o\n\n";
    close_out h
        
end
