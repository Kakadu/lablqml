open Parser
open Core
open Printf
open SuperIndex
type options = { 
  mutable reparse_xml: bool;
  mutable input_file: string;
  mutable print_virtuals: bool;
  mutable nocpp: bool;
  mutable noml: bool;
  mutable reparse_base: bool;
  mutable base: Parser.namespace * index_data SuperIndex.t * G.t * SuperIndex.key Core_queue.t;
  mutable out_dir: string;
  mutable includes: string list
}
let options = { reparse_xml= false; 
		input_file= "../aaa.xml";
		print_virtuals= false;
		nocpp=false;
		noml=false;
		reparse_base=false;
		base=(empty_namespace, SuperIndex.empty, G.create (), Core.Core_queue.create ());
		out_dir = "./out";
		includes = ["/usr/include/qt4"]
	      }

(* TODO: experiment with Gc max heap size to speedup generator *)

open Core_arg
let () = Core_arg.parse [
  ("-xml", Unit (fun () -> (options.reparse_xml <- true)), "reparse xml file aaa.xml");
  ("-nocpp", Unit (fun () -> (options.nocpp <- true)), "don't generate cpp");
  ("-file" , String (fun s -> options.input_file <- s), "XML file of classess");
  ("-noml", Unit (fun () -> (options.noml <- true)), "don't generate ml");
  ("-I", String (fun s -> options.includes <- s :: options.includes), "add include to cpp generated files");
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
    let (index,g,q) = build_superindex root_ns in
    printf "Queue length is %d\n" (Core_queue.length q);
    print_endline "Index builded";
    let index = Filter.filter_constrs index in
    options.base <- (root_ns, index, g, q);

    let ch = open_out "superindex.log" in
    to_channel  index ch;
    close_out ch;

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
  try
    if not options.nocpp then begin
      let (_,index,graph,q) = options.base in
      let open CppGenerator in
	  print_endline "generating C++ code";
	  (new cppGenerator ~graph ~includes:options.includes options.out_dir index)#generate_q q
    end; 
    if not options.noml then begin
      let (_,index,graph,q) = options.base in
      let open OcamlGenerator in
	  print_endline "generating OCaml code";
	  (new ocamlGenerator graph options.out_dir index )#generate q
    end
  with e -> print_endline (Exn.backtrace ()); raise e
;;
main ();;
