open Parser
open Core
open Printf
open SuperIndex
type options = { 
  mutable print_virtuals: bool;
  mutable nocpp: bool;
  mutable noml: bool;
  mutable reparse_base: bool;
  mutable base: Parser.namespace * index_data SuperIndex.t * G.t *
  SuperIndex.Key.t Core_queue.t;
  mutable includes: string list;
  mutable input_file : string option;
}
let options = {
    print_virtuals= false;
    nocpp=false;
    noml=false;
    reparse_base=false;
    base=(empty_namespace, SuperIndex.empty, G.create (), Core.Core_queue.create ());
    includes = ["`pkg-config --cflags QtGui`"];
    input_file = None;
  }

(* parse and save file *)
let main out_dir = 
  let root =
    match options.input_file with
    | Some input_file ->
        print_endline "parsing xml file";
        let root = Simplexmlparser.xmlparser_file input_file in
        let xml_out = open_out "xml.backup" in
        Marshal.to_channel xml_out root [];
        Out_channel.close xml_out;
        print_endline "XML tree backuped"; root
    | None ->
        let ch = open_in "xml.backup" in
        let root: Simplexmlparser.xml list = Marshal.from_channel ch in
        close_in ch;
        print_endline "XML tree restored from backup";
        root
  in
  if (None <> options.input_file) || options.reparse_base then begin
    let root_ns = List.map ~f:build root |> List.hd_exn in
    print_endline "building superindex";
    let (index,g,q) = build_superindex root_ns in
    printf "Queue length is %d\n" (Core_queue.length q);
    print_endline "Index builded";
    let index = Filter.filter_constrs index in
    options.base <- (root_ns, index, g, q);

    let ch = open_out "superindex.log" in
    to_channel  index ch;
    Out_channel.close ch;
(*
    let ch = open_out "tree.backup" in
    Marshal.to_channel ch options.base [];
    Out_channel.close ch *)
  end else begin 
    let ch = open_in "tree.backup" in
    options.base <- Marshal.from_channel ch;
    close_in ch;
    print_endline "Index restored."
  end;
  print_endline "HERE";
  if not options.nocpp then begin
    let (_,index,graph,q) = options.base in
    let open CppGenerator in
    print_endline "generating C++ code";
    (new cppGenerator ~graph ~includes:options.includes out_dir index)#generate_q q
  end else begin
    print_endline "Generation of C++ code skipped"
  end; 

  if not options.noml then begin
    let (_,index,graph,q) = options.base in
    let open OcamlGenerator in
    print_endline "generating OCaml code";
    (new ocamlGenerator graph out_dir index )#generate q
  end else begin
    print_endline "Generation of OCaml code skipped"
  end
(* TODO: experiment with Gc max heap size to speedup generator *)

open Core_arg
let () =
  let l = ref [] in
  Core_arg.parse [
  ("-xml", String (fun s -> options.input_file <- Some s), "<input.xml> reparse xml file");
  ("-nocpp", Unit (fun () -> options.nocpp <- true), "don't generate cpp");
  ("-noml", Unit (fun () -> options.noml <- true), "don't generate ml");
  ("-I", String (fun s -> options.includes <- s :: options.includes), "<path> add include to generated c++ files");
  ("-virt", Unit (fun () -> options.print_virtuals <- true), "print virtual methods of all classes and return")
  ] (fun s -> l := s :: !l) "Usage: main.native [options] <out_dir>\nAvailable options:";
  match List.rev !l with
  | out_dir::[] ->
    begin try
      main out_dir
    with e -> print_endline (Exn.backtrace ()); exit 1 end
  | _ ->
    prerr_endline "Not enough arguments, try -help"; exit 2



