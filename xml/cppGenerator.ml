open Core
open Core.Common
open Generators
open SuperIndex
open Printf
open Parser

module Q = Core_queue
module List = Core_list
module String = Core_string

let iter = List.iter

class cppGenerator ~includes dir index = object (self)
  inherit abstractGenerator index as super

  method gen_stub ~prefix classname (args: func_arg list) ?res_n_name  h =
    try
      let is_constr = Option.is_none res_n_name in
      let get_name is_byte = cpp_stub_name ~classname args ?res_n_name ~is_byte in 
      let native_func_name = get_name false in
      let () = 
	let args_str = args |> List.map ~f:string_of_arg |> String.concat ~sep:"," in
	match res_n_name with
	| Some (res_type,name) -> 
	  fprintf h "// method %s %s(%s)\n" (string_of_type res_type) name args_str
	| None ->
	  fprintf h "// constructor %s(%s)\n" classname args_str
      in
      let argslen = List.length args + (if is_constr then 0 else 1) in
      if argslen > 10 then raise BreakSilent;
(*      fprintf h "// argslen = %d\n" argslen; *)
      let (args,argnames) =
	let argnames = List.mapi args ~f:(fun i _ -> sprintf "arg%d" i) in
	if not is_constr then
	  ((ptrtype_of_classname classname, None) :: args, "self"::argnames)
	else
	  (args, argnames)
      in
      assert (List.length argnames = (List.length args));
      let arg_casts = List.map2_exn argnames args ~f:(fun name (t,default) -> 
	self#fromCamlCast (self#index) (unreference t) ~default name
      ) in
      let arg_casts = List.map arg_casts ~f:(function Success s  -> s
	| CastError _
	| CastValueType _
	| CastTemplate _  -> raise BreakSilent) in

      let res_type = match res_n_name with
	  | None -> ptrtype_of_classname classname
	  | Some (res,_) -> res
      in
      let () = 
	if res_type <> void_type then
	match self#fromCamlCast self#index (unreference res_type) ~default:None "resname" with
	| Success _ -> ()
	| _ -> raise BreakSilent
      in

      fprintf h "value %s(" native_func_name;
      List.map argnames ~f:(fun x -> "value "^x) |> String.concat ~sep:"," |> fprintf h "%s";
      fprintf h ") {\n";

      if argslen>5 then (
	let l1,l2 = headl 5 argnames in
	fprintf h "  CAMLparam5(%s);\n"               (l1 |> String.concat ~sep:",");
	fprintf h "  CAMLxparam%d(%s);\n" (argslen-5) (l2 |> String.concat ~sep:",")
      ) else (
	fprintf h "  CAMLparam%d(%s);\n" argslen (argnames |> String.concat ~sep:",")
      );
      let is_proc = res_type=void_type in
      if not is_proc then
	fprintf h "  CAMLlocal1(_ans);\n";

      List.iter arg_casts ~f:(fun s -> fprintf h "  %s\n" s);
      let argsCall = 
	let lst = if not is_constr then (
	  assert (List.length argnames >0);
	  List.tl_exn argnames
	) else argnames in
	List.map lst ~f:((^)"_") |> String.concat ~sep:", " in
      if is_constr then (
	let ns_prefix = match prefix with
	  | [] -> ""
	  | lst -> String.concat ~sep:"::" lst ^ "::" in
	let full_classname = ns_prefix ^ classname in
	fprintf h "  %s* ans = new %s(%s);\n" full_classname full_classname argsCall;
	fprintf h "  _ans = (value) ans;\n";
	fprintf h "  CAMLreturn(_ans);\n"
      ) else begin
	let methname = match res_n_name with Some (_,x) -> x | None -> assert false in
	if is_proc then (
	  fprintf h "  _self -> %s(%s);\n" methname argsCall;
	  fprintf h "  CAMLreturn(Val_unit);\n"
	) else begin
	  let res = unreference res_type in
	  let ans_type_str = match pattern index (res,None) with
	    | EnumPattern (e,key) -> snd key
	    | _ -> string_of_type res 
	  in
	  let resCast = match self#toCamlCast (unreference res,None) "ans" "_ans" with 
	    | Success s -> s
	    | CastError _ | CastValueType _ | CastTemplate _ -> raise (BreakSilent)
	  in
	  fprintf h "  %s ans = _self -> %s(%s);\n" ans_type_str methname argsCall;
	  fprintf h "  %s\n" resCast;
	  fprintf h "  CAMLreturn(%s);\n"
	      (match pattern index (res,None) with
		| ObjectPattern -> sprintf " (ans) ? Val_some(%s) : Val_none" "_ans"
		| _ -> "_ans")
	end
      end;
      fprintf h "}\n";
      if argslen > 5 then begin
	let stub_name = get_name true in
	fprintf h "value %s(value *argv, int) {\n" stub_name;
        fprintf h "  return %s(\n    %s);\n" (get_name false)
	  (List.init argslen ~f:(fun x -> x) 
	      |> List.map ~f:(fun i-> sprintf "argv[%d]" i) 
	      |> String.concat ~sep:",");
	fprintf h "}\n\n"
      end
      else fprintf h "\n"
    with
      | BreakSilent -> ()
      | BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method makefile dir lst = 
    let lst = List.stable_sort ~cmp:String.compare lst in
    let h = open_out (dir ^/ "Makefile") in
    fprintf h "INCLUDES=-I./../../ -I.";
    List.iter includes ~f:(fun s -> fprintf h " -I%s" s);
    fprintf h "\n";
    fprintf h "GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED $(INCLUDES) \n\n";
    fprintf h "C_QTOBJS=%s\n\n" (List.map ~f:(fun s -> s ^ ".o") lst |> String.concat ~sep:" ");
    fprintf h ".SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx\n\n";
    fprintf h ".cpp.o:\n\t$(GCC) -c -I`ocamlc -where` -I.. $(COPTS) -fpic $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "lablqt: $(C_QTOBJS)\n\n";
(*    fptintf h ".PHONY: all"; *)
    close_out h

  method private prefix = dir ^/ "cpp"
  
  method is_abstr_class c = begin
    let ans = ref false in
    let f m = match m.m_modif with `Abstract -> ans:=true | _ -> () in
    MethSet.iter c.c_meths ~f;
    MethSet.iter c.c_slots ~f;
    !ans
  end
 
  method gen_class ~prefix ~dir c : string option = 
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then begin
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key);
      None
    end else if skipClass ~prefix c then begin 
      printf "skipping class %s\n" c.c_name;
      None
    end else begin
      let classname = c.c_name in
      let subdir = (String.concat ~sep:"/" (List.rev prefix)) in
      let dir =  dir ^/ subdir in
      ignore (Sys.command ("mkdir -p " ^ dir ));
      let filename = dir ^/ classname ^ ".cpp" in
      let h = open_out filename in
      fprintf h "#include <Qt/QtOpenGL>\n";
      fprintf h "#include \"headers.h\"\nextern \"C\" {\n";
      fprintf h "#include \"enum_headers.h\"\n";
      if self#is_abstr_class c then
	fprintf h "//class has pure virtual members - no constructors\n"
      else begin
	let f args =
	  let m = meth_of_constr ~classname args in
          if is_good_meth ~index ~classname m then 
	    self#gen_stub ~prefix classname args ?res_n_name:None h
	in
	iter ~f c.c_constrs 
      end;
      let f m = 
	if (is_good_meth ~classname ~index m) && (m.m_declared = classname) then
	  self#gen_stub ~prefix classname m.m_args ?res_n_name:(Some (m.m_res, m.m_name)) h
      in
      MethSet.iter ~f c.c_meths;
      MethSet.iter ~f c.c_slots; 

      fprintf h "}  // extern \"C\"\n";
      close_out h;
      Some (if subdir = "" then classname else subdir ^/ classname)
    end
  
  method genProp classname h (name,r,w) = ()
  
  method gen_enum_in_ns ~key ~dir:string {e_name;e_items;e_access;e_flag_name} = 
    if not (SuperIndex.mem index key) then None
    else if not (is_public e_access) then None
    else begin
      let prefix = fst key |> List.tl_exn in
      let subdir = (String.concat ~sep:"/" (List.rev prefix)) in
      let dir = self#prefix ^/ subdir in
      ignore (Sys.command ("mkdir -p " ^ dir ));
      let filename = String.concat ~sep:"_" ("enum" :: (fst key|> List.rev)) in
      let h = open_out (dir ^/ filename ^".cpp") in
      fprintf h "//enum %s %s\n\n" e_name (List.to_string (fun x -> x) e_items);

      fprintf h "#include <Qt/QtOpenGL>\n";
      fprintf h "#pragma GCC diagnostic ignored \"-Wswitch\"\n";
      fprintf h "#include \"headers.h\"\nextern \"C\" {\n";
      let (fname1,fname2) = enum_conv_func_names key in
      let s =  List.tl_exn (fst key) |> List.rev |> String.concat ~sep:"::" in
      fprintf h "%s %s(value v) {\n" (snd key) fname1;

      List.iter e_items ~f:(fun e ->
	fprintf h "  if (v==caml_hash_variant(\"%s\")) return %s::%s;\n" e s e
      );
      fprintf h "  printf(\"%s\");\n" "if u see this line, the thereis a bug in enum generation";
      fprintf h "  return %s::%s;\n" s (List.hd_exn e_items);
      fprintf h "}\n\n";

      fprintf h "value %s(%s e) {\n" fname2 (snd key);
      fprintf h "  switch (e) {\n";

      List.iter e_items ~f:(fun e ->
	fprintf h "    case %s::%s: return hash_variant(\"%s\");\n" s e e
      );
      fprintf h "  }\n";
      fprintf h "  printf(\"%s\");\n" "if u see this line, the thereis a bug in enum generation";
      fprintf h "  return %s::%s;\n" s (List.hd_exn e_items);
      fprintf h "\n}\n\n}\n";
      close_out h;
      Some (if subdir = "" then filename else subdir ^/ filename)
    end

  method generate_q q = 
    ignore (Sys.command ("rm -rf " ^ self#prefix ^"/*") );
    let classes = ref [] in
    let enums = ref [] in
    Q.iter q ~f:(fun key -> match SuperIndex.find_exn index key with
      | Enum e -> begin
	match self#gen_enum_in_ns ~key ~dir:(self#prefix) e with
	  | Some s -> Ref.replace enums (fun lst -> (key,s)::lst)
	  | None -> ()
      end
      | Class (c,_) ->  begin 
	match self#gen_class ~prefix:(fst key |> List.tl_exn) ~dir:(self#prefix) c with
	  | Some s -> classes := s :: !classes
	  | None -> ()
      end    
    );
    print_endline "Generating makefile";
    self#makefile self#prefix (List.map ~f:snd !enums @ !classes );
    let enums_h = open_out (self#prefix ^/ "enum_headers.h") in
    List.iter !enums ~f:(fun ((_,fullname) as key,_) ->
      let (fname1,fname2) = enum_conv_func_names key in
      fprintf enums_h "extern %s %s(value);\n" fullname fname1;
      fprintf enums_h "extern value %s(%s);\n\n" fname2 fullname
    );
    close_out enums_h;

end
