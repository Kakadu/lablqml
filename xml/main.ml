open Parser
open Core
open SuperIndex
type options = { mutable reparse_xml: bool;
		 mutable input_file: string;
		 mutable print_virtuals: bool;
		 mutable nocpp: bool;
		 mutable noml: bool;
		 mutable reparse_base: bool;
		 mutable base: (Parser.namespace * index_data SuperIndex.t * G.t);
		 mutable out_dir: string
	       }
let options = { reparse_xml= false; 
		input_file= "/home/kakadu/mand/prog/lablqt/aaa.xml";
		print_virtuals= false;
		nocpp=false;
		noml=false;
		reparse_base=false;
		base=(empty_namespace, SuperIndex.empty, G.create ());
		out_dir = "./out"
	      }

open Core_arg
let () = Core_arg.parse [
  ("-xml", Unit (fun () -> (options.reparse_xml <- true)), "reparse xml file aaa.xml");
  ("-base", Unit (fun () -> (options.reparse_xml <- true)), "reparse xml file aaa.xml");
  ("-nocpp", Unit (fun () -> (options.nocpp <- true)), "don't generate cpp");
  ("-noml", Unit (fun () -> (options.noml <- true)), "don't generate ml");
  ("-virt", Unit (fun () -> options.print_virtuals <- true), 
   "print virtual meths of all classes and return")
  ] (fun _ -> print_endline "fuck. anonymous function") "usage_msg"

let root = ref [Simplexmlparser.PCData ""];;
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
  if options.reparse_xml or options.reparse_base then begin
    let root_ns = List.map build !root |> List.hd in
    print_endline "building superindex";
    let (index,g) = build_superindex root_ns in
    print_endline "Index builded";
    options.base <- (root_ns, index, g);
    let ch = open_out "tree.backup" in
    Marshal.to_channel ch options.base [];
    close_out ch
  end else begin 
    let ch = open_in "tree.backup" in
    options.base <- Marshal.from_channel ch;
    close_in ch;
    print_endline "Index restored."
  end
;;
main ();;


let main () = 

  if not options.nocpp then begin
    let (root,index,_) = options.base in
    let open CppGenerator in
    print_endline "generating C++ code";
    (new cppGenerator options.out_dir index)#generate root
  end; 
  if not options.noml then begin
    let (root,index,g) = options.base in
    let open OcamlGenerator in
    print_endline "generating OCaml code";
    (new ocamlGenerator options.out_dir (index,g) )#generate root
  end
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
