open Core
open Core.Std
open Parser
open Printf

let procedures = 
  [ ("protectedConstructors",     Procedures.protectedConstructors)
  ; ("privateNonAbstractMethods", Procedures.noPrivateNonAbstract)
  ; ("noDestructors",             Procedures.noDestructors)
  ; ("noMETA",                    Procedures.noMETAfuncs)
  ; ("noTemplates",               Procedures.noTemplates)
  ; ("filterMethods",             Procedures.filter_methods)
  ]

let names  = ref (Core_set.empty ~comparator:Core_string.comparator )
let xml_name = ref "../aaa.xml"
let out_name = ref "out.xml"
let do_procedures = ref (List.map ~f:fst procedures)

let () = Arg.parse [ 
  ("-in",  Arg.String (fun s -> xml_name := s), "input xml file");
  ("-out", Arg.String (fun s -> out_name := s), "output xml file");
  ("-app", Arg.String 
    (fun s -> do_procedures := !do_procedures @ (String.split s ~on:',') ), "procedures for simplification")
 ]
  (fun name -> names := Core_set.add !names name) 
  "Usage: xmltool -in [XML file] -out [XML file]"

let root = Simplexmlparser.xmlparser_file !xml_name |> List.hd_exn
module Printer = Xml_print.Make_simple(Xml)(struct let emptytags=[] end) 

let root_ns = Parser.build root

let (superIndex,g) = SuperIndex.build_graph root_ns

open Simplexmlparser

module VertexComparatorPre : Core.Comparator.Pre = struct 
  
  type t = SuperIndex.V.t
  let sexp_of_t = SuperIndex.V.sexp_of_t
  let t_of_sexp = SuperIndex.V.t_of_sexp
  let compare = SuperIndex.V.compare
end 
module VertexComparator = Comparator.Make (VertexComparatorPre)
module VertexComparator2= Comparator.Make1(struct
  type 'a t = SuperIndex.V.t
  let compare = SuperIndex.V.compare
  let sexp_of_t = SuperIndex.V.sexp_of_t
end)
(* Maybe I can write
 * module VertexComparator = Core.Comparator.Make (V) *)

let () = 
  (* Set for accumulating answer *)
  let start_set : (_,_) Core_set.t ref = 
    ref (Core_set.empty ~comparator: VertexComparator2.comparator) 
  in
  (* put classes described in command line to answer*)
  SuperIndex.G.iter_vertex (fun v ->
    let name = SuperIndex.G.vertex_name v in
    if Core_set.mem !names name then start_set:= Core_set.add !start_set v
  ) g;
  (* there we will accumulate classes with their parent classes *)
  let ans_vs : (_,_) Core_set.t ref = ref !start_set in
  let rec loop set = 
    if Core_set.is_empty set then () else begin
      let parents = ref (Core_set.empty ~comparator:VertexComparator2.comparator) in
      Core_set.iter set ~f:(SuperIndex.G.iter_pred (fun  v -> parents := Core_set.add !parents v ) g);
      ans_vs:= Core_set.union !ans_vs !parents;
      loop !parents
    end
  in
  loop !start_set;

  (*  Set.iter (fun x -> print_endline (G.vertex_name x)) !ans_vs; *)
  let names = 
    Core_set.union (Core_set.map ~f:SuperIndex.G.vertex_name !ans_vs ~comparator:Core_string.comparator) !names in
  
  let lst = match root with
    | Element ("code",_,lst) -> lst
    | _ -> assert false
  in
  (* get list of XML nodes to put to result *)
  let ans_lst = ref (List.filter lst ~f:(fun e -> match e with
    | Element (_,attr,_) ->
      let name = List.Assoc.find_exn attr "name" in
      Core_set.mem names name
    | _ -> false
  ) ) in
  List.iter !do_procedures ~f:(fun name ->
    printf "Applying procedure `%s`\n" name;
    let f = List.Assoc.find_exn procedures name in
    ans_lst := List.filter_map !ans_lst ~f
  );
  let xml = Element ("code",[], !ans_lst) in
  let out_ch = open_out !out_name in
  printf "Writing out xml to `%s`...\n" !out_name;
  let out s = Printf.fprintf out_ch "%s" s in
  Simplexmlwriter.print ~out xml;
  Out_channel.close out_ch;
  
  let () = 
    let module StringSet = String.Set in
    let module StringLabeledGraph = SuperIndex.StringLabeledGraph in
    let declared_classes = ref StringSet.empty in
    printf "Generating result graph\n%!";
    let root_ns = Parser.build xml in

    let g = SuperIndex.StringLabeledGraph.create () in 

    let skip_argname s : bool = 
      (* skip primitive variables*)
      (s |> List.mem 
          ["QVariant";"uint";"int";"bool";"void";"qint64";"long";"uchar";"qreal";"char";"WId";"QString"]) 
      (* skip enums *)
      || ( String.is_prefix ~prefix:"Qt::" s && (
        try 
          SuperIndex.is_enum_exn (SuperIndex.NameKey.key_of_fullname s) !superIndex
        with Not_found -> true
      ) )
    in
    let rec iter_ns ns = 
      List.iter ~f:iter_ns ns.ns_ns;
      List.iter ~f:iter_class ns.ns_classes;
    and iter_class c = 
      StringLabeledGraph.add_vertex g c.c_name;
      declared_classes := StringSet.add !declared_classes c.c_name;
      MethSet.iter ~f:(iter_meth c.c_name) c.c_meths
    and iter_meth class_v {m_args; m_res;_} =
      let xs = m_args |> List.map ~f:(fun a -> a.arg_type) in
      List.iter (m_res::xs) ~f:(fun arg_type ->
        let new_name = 
          let s = arg_type.t_name in
          if Str.string_match (Str.regexp "QList<[a-zA-Z]*\\*>") s 0 
          then String.drop_suffix  (String.drop_prefix s 6) 2
          else s
        in
        if (not (skip_argname new_name)) && (arg_type.t_params=[]) && 
          (not (String.is_prefix new_name ~prefix:"QtPrivate") ) 
        then begin
          StringLabeledGraph.add_vertex g new_name;
          StringLabeledGraph.add_edge g class_v new_name
        end
      )
    in
    iter_ns root_ns;
    
    let h = open_out "./qwe.dot" in
    let () = GraphPrinter.output_graph h g 
      ~is_declared:(StringSet.mem !declared_classes) in
    Out_channel.close h
  in
  ()      
  















