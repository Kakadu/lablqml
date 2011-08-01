let filename = "/home/kakadu/mand/prog/lablqt/aaa.xml"
open Simplexmlparser

let root = Simplexmlparser.xmlparser_file filename
open Parser

(*    
let dumpdot filename nslst = 
  let h = open_out filename in
  let print = Printf.fprintf h in  
  print "digraph test {\n";
  List.iter (fun ns ->
    
    Printf.fprintf h "%s;\n" ns.ns_name
  ) nslst;
  print "}\n";
  close_out h
*)
let out_dir = "./out/" 
let rec headl c lst =
  let rec helper c h tl = 
    if c=0 then (List.rev h,tl)
    else match tl with
      | hh::ll -> helper (c-1) (hh::h) ll
      | [] -> raise (Failure "headl")
  in
  helper c [] lst

open Generators

module List = struct
  include List
  let find_opt func lst = try 
			    Some (find func lst)
    with Not_found -> None
end

class cppGenerator dir (index:Parser.indexItem Parser.Index.t) =
  let iter = List.iter in
  let fprintf = Printf.fprintf in
  object (self)
  inherit abstractGenerator index as super

  method genMeth classname h (meth_name,lst,res,acce,modlst) = 
    let meth_name = if (startswith ~prefix:(classname^"::") meth_name) then
	Str.string_after meth_name (String.length classname+2) else meth_name in
(*    print_endline ("generating method " ^ classname ^ "::"^ meth_name); *)
    try
      let skipStr = ("skipped " ^ classname ^ "::" ^ meth_name) in
      if (goodMeth meth_name res lst acce modlst) then 
	raise (BreakS skipStr);
      
      fprintf h "// method %s %s(%s)\n"
	(match res with Some s -> self#type2str s | None -> "<none>") meth_name
	(List.map (self#type2str $ fst) lst |> String.concat ",");


      let argnames = ref [] in
      for i=List.length lst downto 1 do argnames := ("arg"^(string_of_int i)) :: !argnames done;
      let argnames = "self" :: !argnames in      
      let len = List.length argnames in
      if len > 10 then
	raise (BreakS (skipStr ^ ": to many arguments"));

      let lst2 = ({t_name=classname; t_is_ref=false; t_is_const=false; t_indirections=1;t_params=[] },None)
	:: lst in
      let argCasts = List.map2 (fun name (t,def) -> 
	self#fromCamlCast (self#index) t ~default:def name
      ) argnames lst2 in
      let fail = List.find_opt (function CastError _ -> true | _ -> false) argCasts in

      match fail with
	| Some (CastError s) -> raise (BreakS s)
	| None -> begin
	  let cpp_func_name = self#cppFuncName classname meth_name lst in
	  fprintf h "value %s(%s) {\n" cpp_func_name 
	    (List.map (fun s->"value "^s) argnames |> String.concat ", ");
	  
	  
	  if len>5 then (
	    let l1,l2 = headl 5 argnames in
	    fprintf h "  CAMLparam5(%s);\n" (                   l1 |> String.concat ",");
	    fprintf h "  CAMLlocal5(%s);\n" (List.map ((^) "_") l1 |> String.concat ",");
	    fprintf h "  CAMLxparam%d(%s);\n" (len-5) (                  l2 |> String.concat ",");
	    fprintf h "  CAMLlocal%d(%s);\n" (len-5) (List.map ((^)"_") l2 |> String.concat ",")	     
	  ) else (
	    fprintf h "  CAMLparam%d(%s);\n" len (                  argnames |> String.concat ",");
	    fprintf h "  CAMLlocal%d(%s);\n" len (List.map ((^)"_") argnames |> String.concat ",")
	  );

	  let argCasts = List.map (function Success s -> s| _ -> assert false) argCasts in
	  List.iter (fun s -> fprintf h "  %s\n" s) argCasts;
	  fprintf h "\n";
	  fprintf h "}\n"
	end
	| _ -> assert false

    with BreakS str -> ( fprintf h "// %s\n" str;
			 print_endline str )

  method genConstr classname h (lst,policy,modif) = ()

  method makefile dir lst = 
    let h = open_out (dir ^ "/Makefile") in
    fprintf h "GCC=g++ -c -pipe -g -O2 -Wall -W -D_REENTRANT -DQT_GUI_LIB -DQT_CORE_LIB -DQT_SHARED -I/usr/include/qt4/\n\n";
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
      fprintf h "#include <Qt/QtGui>\n#include \"headers.h\"\nextern \"C\" {\n";
      iter (self#genProp c.c_name h) c.c_props;
      iter (self#genSignal c.c_name h) c.c_sigs;
      iter (self#genSlot c.c_name h) c.c_slots;
      iter (self#genMeth c.c_name h) c.c_meths;
      iter (self#genEnumOfClass c.c_name h) c.c_enums;
      iter (self#genConstr classname h) c.c_constrs;
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

let () = 
  let ans = List.map build root in
  print_endline "\ngenerating index\n";
  let index = build_index ans in
(*  print_endline "printing keys";
  Index.print_keys index;
  print_endline "\n*** end of index\n";
*)
  print_endline "\ngenerating cpp code\n";
  let _ = (new cppGenerator out_dir index)#generate ans in
  ()

