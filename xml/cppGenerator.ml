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

class cppGenerator dir index = object (self)
  inherit abstractGenerator index as super

  method genMeth classname h m =
    let methname = m.m_name in
    let res = m.m_res and lst= m.m_args in
(*    let meth_name = fixElementName ~classname meth_name in *)
    try
      let skipStr = sprintf "skipped %s::%s" classname methname in
      if not (is_good_meth ~classname m) then 
	breaks (sprintf "skipped %s --- not is_good_method." (string_of_meth m));
      
      fprintf h "// method %s \n" (string_of_meth m);

      let isProc = (res.t_name = "void") in

      let resCast = if isProc then ""
	else begin match self#toCamlCast (unreference res) "ans" "_ans" with 
	  | CastError s -> raise (BreakS (sprintf "cant cast return type (%s): " (string_of_type res) ) )
	  | CastValueType name -> raise (BreakS ("Cast error. Return type is value-type: " ^ name))
	  | CastTemplate str -> raise (BreakS ("Cast error. return tppe is a template: "^ str))
	  | Success s -> s 
	end
      in
      let argnames = List.mapi lst ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      let argnames = "self" :: argnames in      
      let len = List.length argnames in
      if len > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      let type_of_clas = 
	{ t_name=classname; t_is_ref=false; t_is_const=false; t_indirections=1; t_params=[] } in
      let lst2 = (type_of_clas, None) :: lst in
      let arg_casts = List.map2_exn argnames lst2 ~f:(fun name (t,default) -> 
	self#fromCamlCast (self#index) (unreference t) ~default name
      ) in
      let fail = List.find arg_casts ~f:(function Success _ -> false | _ -> true) in

      match fail with
	| Some (CastError s) -> raise (BreakS s)
	| Some (CastValueType name) -> raise (BreakS ("Casting value-type: " ^ name))
	| Some (CastTemplate str) -> raise (BreakS ("Casting template: "^ str))
	| None -> begin
	  let cpp_func_name = cpp_func_name ~classname ~methname lst in
	  fprintf h "value %s(%s) {\n" cpp_func_name 
	    (List.map argnames ~f:(fun s->"value "^s) |> String.concat ~sep:", ");
	  	  
	  if len>5 then (
	    let l1,l2 = headl 5 argnames in
	    fprintf h "  CAMLparam5(%s);\n"           (l1 |> String.concat ~sep:",");
	    fprintf h "  CAMLxparam%d(%s);\n" (len-5) (l2 |> String.concat ~sep:",") 
	  ) else (
	    fprintf h "  CAMLparam%d(%s);\n" len (argnames |> String.concat ~sep:",")
	  );
	  if not isProc then 
	    fprintf h "  CAMLlocal1(_ans);\n";
	  
	  let arg_casts = List.map arg_casts ~f:(function Success s -> s| _ -> assert false) in
	  List.iter arg_casts ~f:(fun s -> fprintf h "  %s\n" s);
	  let argsCall = List.map (List.tl_exn argnames) ~f:((^)"_") |> String.concat ~sep:", " in
	  if isProc then (
	    fprintf h "  _self -> %s(%s);\n" methname argsCall;
	    fprintf h "  CAMLreturn(Val_unit);\n" 
	  ) else (
	    fprintf h "  %s ans = _self -> %s(%s);\n" 
	      (unreference res |> string_of_type ) methname argsCall;
	    fprintf h "  %s\n" resCast;
	    fprintf h "  CAMLreturn(_ans);\n" 
	  );	  
	  fprintf h "}\n\n"
	end
	| _ -> assert false
    with BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method genConstr classname h lst =
(*    print_endline ("generating constructor of " ^ classname); *)
    try
      let skipStr = sprintf "skipped %s::%s" classname classname in
      let fake_meth = meth_of_constr ~classname lst in
      if not (is_good_meth ~classname fake_meth) then 
	breaks (sprintf "Skipped constructor %s\n" (string_of_meth fake_meth) );
      
      fprintf h "// constructor `%s`(%s)\n"
	classname (List.map lst ~f:(string_of_type $ fst) |> String.concat ~sep:",");

      let argnames = List.mapi lst ~f:(fun i _ -> "arg" ^ (string_of_int i)) in
      let len = List.length argnames in
      if len > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      let argCasts = List.map2_exn ~f:(fun name (t,default) -> 
	self#fromCamlCast (self#index) (unreference t) ~default name
      ) argnames lst in
      let fail = Core_list.find argCasts ~f:(function Success _ -> false | _ -> true) in

      match fail with
	| Some (CastError s) -> breaks s
	| Some (CastValueType name) -> raise (BreakS ("Casting value-type: " ^ name))
	| Some (CastTemplate str) -> raise (BreakS ("Casting template: "^ str))
	| None -> begin
	  let cpp_func_name = cpp_func_name ~classname ~methname:classname lst in
	  fprintf h "value %s(%s) {\n" cpp_func_name 
	    (List.map ~f:(fun s->"value "^s) argnames |> String.concat ~sep:", ");
	  	  
	  if len>5 then (
	    let l1,l2 = headl 5 argnames in
	    fprintf h "  CAMLparam5(%s);\n" (                   l1 |> String.concat ~sep:",");
	    fprintf h "  CAMLxparam%d(%s);\n" (len-5) (                  l2 |> String.concat ~sep:",") 
	  ) else (
	    fprintf h "  CAMLparam%d(%s);\n" len (                  argnames |> String.concat ~sep:",")
	  );
	  fprintf h "  CAMLlocal1(_ans);\n";
	  
	  let argCasts = List.map ~f:(function Success s -> s| _ -> assert false) argCasts in
	  List.iter ~f:(fun s -> fprintf h "  %s\n" s) argCasts;
	  let argsCall = List.map ~f:((^)"_") argnames |> String.concat ~sep:", " in
	  
	  fprintf h "  %s *ans = new %s(%s);\n" classname classname argsCall;
	  fprintf h "  _ans = (value)ans;\n";
	  fprintf h "  CAMLreturn(_ans);\n";	    
	  fprintf h "}\n\n"
	end
	| _ -> assert false
    with BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method makefile dir lst = 
    let lst = List.stable_sort ~cmp:String.compare lst in
    let h = open_out (dir ^/ "Makefile") in
    fprintf h "GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I/usr/include/qt4/ -I./../../ \n\n";
    fprintf h "C_QTOBJS=%s\n\n" (List.map ~f:(fun s -> s ^ ".o") lst |> String.concat ~sep:" ");
    fprintf h ".SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx\n\n";
    fprintf h ".cpp.o:\n\t$(GCC) -c -I`ocamlc -where` $(COPTS) -fpic $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "lablqt: $(C_QTOBJS)\n\n";
(*    fptintf h ".PHONY: all"; *)
    close_out h

  method private prefix = dir ^/ "cpp"

  method gen_class ~prefix ~dir c : string option = 
    let key = NameKey.make_key ~name:c.c_name ~prefix in
    if not (SuperIndex.mem index key) then begin
      printf "Skipping class %s - its not in index\n" (NameKey.to_string key);
      None
    end else if skipClass c then None
    else begin
      let classname = c.c_name in
      let subdir = (String.concat ~sep:"/" (List.rev prefix)) in
      let dir =  dir ^/ subdir in
      ignore (Sys.command ("mkdir -p " ^ dir ));
      let filename = dir ^/ classname ^ ".cpp" in
      let h = open_out filename in
      fprintf h "#include <Qt/QtOpenGL>\n#include \"headers.h\"\nextern \"C\" {\n";
(*      let hasPureVirtualMembers = match Index.find index classname with
	| Some (Class (c,[])) -> false
	| _ -> true
      in
      *)
      if is_abstract_class ~prefix index classname then
	fprintf h "//class has pure virtual members - no constructors\n"
      else
	iter ~f:(self#genConstr classname h) c.c_constrs; 

      MethSet.iter ~f:(fun m -> self#genMeth c.c_name h (fst m) ) c.c_meths_normal;
      iter ~f:(self#genProp c.c_name h) c.c_props;
      iter ~f:(self#genSignal c.c_name h) c.c_sigs;
      iter ~f:(self#genSlot c.c_name h) c.c_slots;
      iter ~f:(self#gen_enumOfClass c.c_name h) c.c_enums;
      fprintf h "}  // extern \"C\"\n";
      close_out h;
      Some (if subdir = "" then classname else subdir ^/ classname)
    end

  method gen_enumOfNs ~prefix ~dir ((_,_):enum) = None
  method gen_enumOfClass classname h (name,lst) = 
    fprintf h "// enum %s\n" name 
  method genSlot classname h (name,lst) = 
    fprintf h "// slot %s(%s)\n" name (List.map ~f:(string_of_type $ fst ) lst |> String.concat ~sep:",")
  method genSignal classname h (name,lst) = ()
  method genProp classname h (name,r,w) = ()

  method generate_q q = 
    let classes = ref [] in
    let rec loop q = 
      match Q.dequeue q with 
	| None -> ()
	| Some key -> begin
	  (match SuperIndex.find_exn index key with
	    | Enum  e -> ()
	    | Class (c,_) -> begin 
	      match self#gen_class ~prefix:(fst key |> List.tl_exn) ~dir:(self#prefix) c with
		| Some s -> classes := s :: !classes
		| None -> ()
	    end
	  );
	  loop q
	end
    in
    loop q;
    print_endline "Generating makefile";
    self#makefile self#prefix !classes

end
