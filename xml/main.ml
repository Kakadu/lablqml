open Simplexmlparser

open Parser
open Core

let out_dir = "./out/" 
let fixElementName ~classname name = 
  match Str.split_delim (Str.regexp "::") name with
    | [s] -> s
    | h::t::[] when h = classname -> t
    | _ -> assert false

let classPointerT classname = {t_name=classname; t_indirections=1; t_is_const=false; t_is_ref = false;
			       t_params=[] }
let is_abstract_class = function
  | s when startswith ~prefix:"QAbstract" s -> true
  | s when endswith ~postfix:"Interface" s -> true
  | "QAccessibleInterface"
  | "QAccessibleImageInterface"
  | "QAccessible" -> true
  | _ -> false


open Generators

class cppGenerator dir (index:indexItem Index.t) =
  let iter = List.iter in
  let fprintf = Printf.fprintf in
  object (self)
  inherit abstractGenerator index as super

  method genMeth classname h (meth_name,lst,res,acce,modlst) = 
    let meth_name = fixElementName ~classname meth_name in
    try
      let skipStr = ("skipped " ^ classname ^ "::" ^ meth_name) in
      if goodMeth ~classname:classname meth_name res lst acce modlst then 
	raise (BreakS skipStr);
      
      fprintf h "// method %s `%s`(%s)\n"
	(self#type2str res) meth_name
	(List.map (self#type2str $ fst) lst |> String.concat ",");

      let isProc = (res.t_name = "void") in

      let resCast = if isProc then ""
	else begin match self#toCamlCast (unreference res) "ans" "_ans" with 
	  | CastError s -> raise (BreakS ("cant cast return type ("^s^"): " ^ (self#type2str res)  ) )
	  | CastValueType name -> raise (BreakS ("Cast error. Return type is value-type: " ^ name))
	  | CastTemplate str -> raise (BreakS ("Cast error. return tppe is a template: "^ str))
	  | Success s -> s 
	end
      in
      
      let argnames = ref [] in
      for i=List.length lst downto 1 do argnames := ("arg"^(string_of_int i)) :: !argnames done;
      let argnames = "self" :: !argnames in      
      let len = List.length argnames in
      if len > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      let lst2 = ({t_name=classname; t_is_ref=false; t_is_const=false; t_indirections=1;t_params=[] },None)
	:: lst in
      let argCasts = List.map2 (fun name (t,def) -> 
	self#fromCamlCast (self#index) (unreference t) ~default:def name
      ) argnames lst2 in
      let fail = Core_list.find ~f:(function Success _ -> false | _ -> true) argCasts in

      match fail with
	| Some (CastError s) -> raise (BreakS s)
	| Some (CastValueType name) -> raise (BreakS ("Casting value-type: " ^ name))
	| Some (CastTemplate str) -> raise (BreakS ("Casting template: "^ str))
	| None -> begin
	  let cpp_func_name = self#cppFuncName classname meth_name lst in
	  fprintf h "value %s(%s) {\n" cpp_func_name 
	    (List.map (fun s->"value "^s) argnames |> String.concat ", ");
	  	  
	  if len>5 then (
	    let l1,l2 = headl 5 argnames in
	    fprintf h "  CAMLparam5(%s);\n" (                   l1 |> String.concat ",");
	    fprintf h "  CAMLxparam%d(%s);\n" (len-5) (                  l2 |> String.concat ",") 
	  ) else (
	    fprintf h "  CAMLparam%d(%s);\n" len (                  argnames |> String.concat ",")
	  );
	  if not isProc then 
	    fprintf h "  CAMLlocal1(_ans);\n";
	  
	  let argCasts = List.map (function Success s -> s| _ -> assert false) argCasts in
	  List.iter (fun s -> fprintf h "  %s\n" s) argCasts;
	  let argsCall = List.map ((^)"_") (List.tl argnames) |> String.concat ", " in
	  if isProc then (
	    fprintf h "  _self -> %s(%s);\n" meth_name argsCall;
	    fprintf h "  CAMLreturn(Val_unit);\n" 
	  ) else (
	    fprintf h "  %s ans = _self -> %s(%s);\n" 
	      (unreference res |> self#type2str ) meth_name argsCall;
	    fprintf h "  %s\n" resCast;
	    fprintf h "  CAMLreturn(_ans);\n" 
	  );	  
	  fprintf h "}\n\n"
	end
	| _ -> assert false
    with BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method genConstr classname h (lst,accessPolicy,modif) =
    print_endline ("generating constructor of " ^ classname);
    try
      let skipStr = ("skipped " ^ classname ^ "::" ^ classname) in
      let () = match accessPolicy with
	| Private | Protected -> raise (BreakS ("not public constructor of class " ^ classname) )
	| Public -> () in
      if goodMeth ~classname:classname classname (classPointerT classname) lst accessPolicy modif then 
	raise (BreakS skipStr);
      
      fprintf h "// constructor `%s`(%s)\n"
	classname (List.map (self#type2str $ fst) lst |> String.concat ",");

      let argnames = ref [] in
      for i=List.length lst downto 1 do argnames := ("arg"^(string_of_int i)) :: !argnames done;
      let argnames = !argnames in      
      let len = List.length argnames in
      if len > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      let argCasts = List.map2 (fun name (t,def) -> 
	self#fromCamlCast (self#index) (unreference t) ~default:def name
      ) argnames lst in
      let fail = Core_list.find argCasts ~f:(function Success _ -> false | _ -> true) in

      match fail with
	| Some (CastError s) -> raise (BreakS s)
	| Some (CastValueType name) -> raise (BreakS ("Casting value-type: " ^ name))
	| Some (CastTemplate str) -> raise (BreakS ("Casting template: "^ str))
	| None -> begin
	  let cpp_func_name = self#cppFuncName classname classname lst in
	  fprintf h "value %s(%s) {\n" cpp_func_name 
	    (List.map (fun s->"value "^s) argnames |> String.concat ", ");
	  	  
	  if len>5 then (
	    let l1,l2 = headl 5 argnames in
	    fprintf h "  CAMLparam5(%s);\n" (                   l1 |> String.concat ",");
	    fprintf h "  CAMLxparam%d(%s);\n" (len-5) (                  l2 |> String.concat ",") 
	  ) else (
	    fprintf h "  CAMLparam%d(%s);\n" len (                  argnames |> String.concat ",")
	  );
	  fprintf h "  CAMLlocal1(_ans);\n";
	  
	  let argCasts = List.map (function Success s -> s| _ -> assert false) argCasts in
	  List.iter (fun s -> fprintf h "  %s\n" s) argCasts;
	  let argsCall = List.map ((^)"_") argnames |> String.concat ", " in
	  
	  fprintf h "  %s *ans = new %s(%s);\n" classname classname argsCall;
	  fprintf h "  _ans = (value)ans;\n";
	  fprintf h "  CAMLreturn(_ans);\n";	    
	  fprintf h "}\n\n"
	end
	| _ -> assert false
    with BreakS str -> ( fprintf h "// %s\n" str; print_endline str )

  method makefile dir lst = 
    let lst = List.fast_sort String.compare lst in
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "GCC=g++ -c -pipe -g -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I/usr/include/qt4/ -I./../../ \n\n";
    fprintf h "C_QTOBJS=%s\n\n" (List.map (fun s -> s ^ ".o") lst |> String.concat " ");
    fprintf h ".SUFFIXES: .ml .mli .cmo .cmi .var .cpp .cmx\n\n";
    fprintf h ".cpp.o:\n\t$(GCC) -c -I`ocamlc -where` $(COPTS) -fpic $<\n\n";
    fprintf h "all: lablqt\n\n";
    fprintf h "lablqt: $(C_QTOBJS)\n\n";
(*    fptintf h ".PHONY: all"; *)
    close_out h

  method private prefix = dir ^ "/cpp/"

  method genClass dir c = 
    if skipClass c then None
    else begin
      let classname = c.c_name in
      let h = open_out (dir ^ "/" ^ classname ^ ".cpp") in
      fprintf h "#include <Qt/QtOpenGL>\n#include \"headers.h\"\nextern \"C\" {\n";
      let hasPureVirtualMembers = match Index.find index classname with
	| Some (Class (c,[])) -> false
	| _ -> true
      in
      
      if is_abstract_class classname then
	fprintf h "//class has pure virtual members - no constructors\n"
      else
	iter (self#genConstr classname h) c.c_constrs;
      iter (self#genMeth c.c_name h) c.c_meths;
      iter (self#genProp c.c_name h) c.c_props;
      iter (self#genSignal c.c_name h) c.c_sigs;
      iter (self#genSlot c.c_name h) c.c_slots;
      iter (self#genEnumOfClass c.c_name h) c.c_enums;
      fprintf h "}  // extern \"C\"\n";
      close_out h;
      Some classname
    end

  method genEnumOfNs _ (_,_) = None
  method genEnumOfClass classname h (name,lst) = 
(*    print_endline ("generating enum " ^ classname ^ "::"^ name); *)
    fprintf h "// enum %s\n" name 
  method genSlot classname h (name,lst) = 
    fprintf h "// slot %s(%s)\n" name (List.map (self#type2str $ fst ) lst |> String.concat ",")
  method genSignal classname h (name,lst) = 
    fprintf h "// signal %s(%s)\n" name (List.map (self#type2str $ fst) lst |> String.concat ",")
  method genProp classname h (name,r,w) = 
    fprintf h "// property %s " name;
    (match r with
      | Some s -> fprintf h "READ %s " s
      | None -> ());
    (match w with
      | Some s -> fprintf h "WRITE %s " s
      | None -> ());
    fprintf h "\n"
end

open OcamlGenerator

type options = { mutable reparse_xml: bool;
		 mutable input_file: string;
		 mutable print_virtuals: bool;
		 mutable nocpp: bool;
		 mutable noml: bool
	       }
let options = { reparse_xml= false; 
		input_file= "/home/kakadu/mand/prog/lablqt/aaa.xml";
		print_virtuals= false;
		nocpp=false;
		noml=true
	      }

open Core_arg
let () = Core_arg.parse [
  ("-xml", Unit (fun () -> (options.reparse_xml <- true)), "reparse xml file aaa.xml");
  ("-nocpp", Unit (fun () -> (options.nocpp <- true)), "don't generate cpp");
  ("-noml", Unit (fun () -> (options.noml <- true)), "don't generate ml");
  ("-virt", Unit (fun () -> options.print_virtuals <- true), 
   "print virtual meths of all classes and return")
  ] (fun _ -> print_endline "fuck. anonymous function") "usage_msg"

let root = ref [PCData ""];;
(* parse and save file *)
let main () = 
  if options.reparse_xml then begin
    print_endline "parsing xml file";
    root := Simplexmlparser.xmlparser_file options.input_file;
    let xml_out = open_out "xml.backup" in
    Marshal.to_channel xml_out !root [];
    close_out xml_out;
    print_endline "XML tree backuped"
  end else begin
    let ch = open_in "xml.backup" in
    let xmltree: Simplexmlparser.xml list = Marshal.from_channel ch in
    close_in ch;
    root := xmltree;
    print_endline "XML tree restored from backup"      
  end;;

main ();;

let main () =
  let root_ns = List.map build !root |> List.hd in
  print_endline "building superindex";
  SuperIndex.build_superindex root_ns
;;
main ();;

(*
let index = ref Index.empty;;
let main () = 
  print_endline "Parsing xml tree..";
  let ans = (List.map build !root) in
  print_endline "\ngenerating index\n";
  index := build_index ans;
  print_endline "index is generated.";
  index := removeClassesWithoutBases !index;
  print_endline "call postBuildIndex";
  index := postBuildIndex !index;
  print_endline "end of post_build index";
  ans
;; 

let ans = main ();;

let main () = 
  if options.print_virtuals then begin
    print_endline "***  virtual meths in classes";
    print_endline "call removeClassesWithoutBases";
    
    let f ~key ~data = match data with
      | Ns _ | Enum _ -> ()
      | Class (c,lst) -> 
	Core_list.iter ~f:(fun (name,args) ->
	  Printf.printf "%s(%s)\n" name (List.map Parser.type2str args |>  String.concat ", " )
	) lst 
    in
    Index.iter !index ~f
end;;
let () = main ();;

let main () =
  if not options.nocpp then begin
    print_endline "\ngenerating cpp code\n";
    let _ = (new cppGenerator out_dir !index)#generate ans in ()
  end
in
main ();;

let main () =
  if not options.noml then begin
    print_endline "\ngenerating OCaml code\n";
    let _ = (new ocamlGenerator out_dir !index)#generate ans in ()
  end
in
main ();;
*)
