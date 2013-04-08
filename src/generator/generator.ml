open Parser
open Core
open Core.Std
open Printf
open SuperIndex

let () = Printexc.record_backtrace true
type options = {
  mutable print_virtuals: bool;
  mutable nocpp: bool;
  mutable noml: bool;
  mutable reparse_base: bool;
  mutable base: Parser.namespace * index_data SuperIndex.t * G.t *
  SuperIndex.Key.t Core_queue.t;
  mutable includes: string list;
  mutable bin_prefix : string;
  mutable input_file : string option;
}

let options = {
    print_virtuals= false;
    nocpp=false;
    noml=false;
    reparse_base=false;
    base=(empty_namespace, SuperIndex.empty, G.create (), Core.Core_queue.create ());
    includes = [];
    bin_prefix = "";
    (*includes = ["`pkg-config --cflags QtGui`"];*)
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
        In_channel.close ch;
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
    Out_channel.close ch
*)
  end else begin
    let ch = open_in "tree.backup" in
    options.base <- Marshal.from_channel ch;
    In_channel.close ch;
    print_endline "Index restored."
  end;
  print_endline "HERE";
  if not options.nocpp then begin
    let (_,index,graph,q) = options.base in
    let open CppGenerator in
    print_endline "generating C++ code";
    (new cppGenerator ~graph ~includes:options.includes ~bin_prefix:options.bin_prefix out_dir index)
      #generate_q q
  end else begin
    print_endline "Generation of C++ code skipped"
  end;

  if not options.noml then begin
    let module V2 = struct
      type t = [ `Single of NameKey.t | `Group of NameKey.t list ]
      let equal x y = match (x,y) with
        | (`Group a, `Group b) -> a=b
        | (`Single a, `Single b) -> a=b
        | _ -> false
      let hash = function
        | `Single s -> NameKey.hash s * 2 + 1
        | `Group xs -> NameKey.hash (List.hd_exn xs) * 2
      let compare x y = match (x,y) with
        | (`Single _, `Group _)  -> -1
        | (`Group _, `Single _)  -> 1
        | (`Group a, `Group b)   -> compare a b
        | (`Single a, `Single b) -> compare a b
      let to_string = function
        | `Single s -> sprintf "Single %s" (NameKey.to_string s)
        | `Group xs -> sprintf "Group %s" (String.concat ~sep:"," (List.map xs ~f:NameKey.to_string))
    end in
    let module Gout = struct
      include Graph.Imperative.Digraph.Concrete(V2)
      let graph_attributes (_:t) = []
      let default_vertex_attributes _ = []
      let get_subgraph _ = None
      let default_edge_attributes _ = []
      let edge_attributes _ = []

      let vertex_name v = match V.label v with
        | `Single s -> NameKey.to_string s
        | `Group xs -> String.concat (List.map xs ~f:NameKey.to_string) (*
        |> Str.global_replace (Str.regexp "::") "_"
        |> Str.global_replace (Str.regexp "<") "_"
        |> Str.global_replace (Str.regexp ">") "_"
        |> Str.global_replace (Str.regexp ",") "_"
        |> Str.global_replace (Str.regexp "*") "_"
        |> Str.global_replace (Str.regexp "&amp;") "_AMP_"*)

      let vertex_attributes v =
        let (x:V2.t) = V.label v in
        [`Label ((V2.to_string x)) ]

    end in
    let module R = Reducer.Make(G)(Gout) in

    let (_,index,main_graph,q) = options.base in
    let components = (* Queue generation *)
      let module T = Graph.Components.Make(G) in
      let graph = G.copy main_graph in
      SuperIndex.iter ~f:(fun ~key ~data ->
        match data with
          | Enum _ -> ()
          | Class(c,_) ->
              let f_arg = fun arg ->
                let dest_v = NameKey.key_of_fullname arg.arg_type.t_name in
                if (G.mem_vertex graph dest_v) && (not (G.mem_edge graph key dest_v))
                then begin
                  (*printf "add_edge %s -> %s\n%!" (NameKey.to_string key) (NameKey.to_string dest_v);*)
                  G.add_edge graph dest_v key
                end
              in
              List.iter c.c_constrs ~f:(List.iter ~f:f_arg);
              let g = fun m ->
                let res_arg = {arg_type=m.m_res;arg_name=None;arg_default=None} in
                List.iter (res_arg::m.m_args) ~f:f_arg;
              in
              MethSet.iter c.c_meths ~f:g;
              MethSet.iter c.c_slots ~f:g;
      ) index;
      let h = open_out "1.dot" in
      GraphPrinter.output_graph h graph;
      Out_channel.close h;
      let gout = R.rebuild graph ~f_cycle:(fun c -> `Group c) ~f_alone:(fun c -> `Single c) in
      let module GraphPrinter2 = Graph.Graphviz.Dot(Gout) in
      let h = open_out "2.dot" in
      GraphPrinter2.output_graph h gout;
      Out_channel.close h;
      let module Top = Graph.Topological.Make(Gout) in
      let xs = Top.fold (fun v acc -> v::acc) gout [] in
      let xs = List.rev xs in
      printf "TopSort result\n";
      List.iter xs ~f:(fun x ->  printf "%s\n%!" (V2.to_string x));
      printf "End of TopSort results\n%!";
      xs |> Queue.of_list
    in
    let open OcamlGenerator in
    print_endline "generating OCaml code";
    (new ocamlGenerator main_graph out_dir index )#generate components
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
  ("-qtloc", String (fun s -> options.bin_prefix <- s), "where to get executables like `moc`");
  ("-I", String (fun s -> options.includes <- s :: options.includes), "<path> add include to generated c++ files");
  ("-virt", Unit (fun () -> options.print_virtuals <- true), "print virtual methods of all classes and return")
  ] (fun s -> l := s :: !l) "Usage: main.native [options] <out_dir>\nAvailable options:";
  match List.rev !l with
  | out_dir::[] -> begin
      try
        main out_dir
      with e ->
        print_endline (Exn.to_string e);
        print_endline (Exn.backtrace ());
        exit 1
    end
  | _ ->
    prerr_endline "Not enough arguments, try -help"; exit 2
