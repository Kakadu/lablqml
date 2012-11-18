open Core
module List = Core_list
(*module Set = Core_set *)
open SuperIndex
open Parser
open Printf

let names  = ref (Core_set.empty ~comparator:Core_string.comparator )
let xml_name = ref "../aaa.xml";;
let out_name = ref "out.xml";;

let () = Arg.parse [ 
  ("-in",  Arg.String (fun s -> xml_name := s), "input xml file");
  ("-out", Arg.String (fun s -> out_name := s), "output xml file")
 ]
  (fun name -> names := Core_set.add !names name) "haha"

let root = Simplexmlparser.xmlparser_file !xml_name |> List.hd_exn
module Printer = Xml_print.Make_simple(Xml)(struct let emptytags=[] end) 

let (_,g) = build_graph (root |> Parser.build)

open Simplexmlparser

module VertexComparatorPre : Core.Comparator.Pre = struct 
  type t = V.t
  let sexp_of_t = V.sexp_of_t
  let t_of_sexp = V.t_of_sexp
  let compare = V.compare
end 
module VertexComparator = Comparator.Make (VertexComparatorPre)
module VertexComparator2= Comparator.Make1(struct
  type 'a t = V.t
  let compare = V.compare
  let sexp_of_t = V.sexp_of_t
end)
(* Maybe I can write
 * module VertexComparator = Core.Comparator.Make (V) *)

let () = 
  let start_set : (_,_) Core_set.t ref = 
    ref (Core_set.empty ~comparator: VertexComparator2.comparator) 
  in
  G.iter_vertex (fun v ->
    let name = G.vertex_name v in
    if Core_set.mem !names name then start_set:= Core_set.add !start_set v
  ) g;
  let ans_vs : (_,_) Core_set.t ref = ref !start_set in
  let rec loop set = 
    if Core_set.is_empty set then () else begin
      let parents = ref (Core_set.empty ~comparator:VertexComparator2.comparator) in
      Core_set.iter set ~f:(G.iter_pred (fun  v -> parents := Core_set.add !parents v ) g);
      ans_vs:= Core_set.union !ans_vs !parents;
      loop !parents
    end
  in
  loop !start_set;

(*  Set.iter (fun x -> print_endline (G.vertex_name x)) !ans_vs; *)
  let names = 
    Core_set.union (Core_set.map ~f:G.vertex_name !ans_vs ~comparator:Core_string.comparator) !names in
  
  let lst = match root with
    | Element ("code",_,lst) -> lst
    | _ -> assert false
  in
  let ans_lst = List.filter lst ~f:(fun e -> match e with
    | Element (_,attr,_) ->
      let name = List.Assoc.find_exn attr "name" in
      Core_set.mem names name
    | _ -> false
  ) in
  let xml = Element ("code",[],ans_lst) in
  let out_ch = open_out !out_name in
  printf "Writing out xml....\n";
  let out s = Printf.fprintf out_ch "%s" s in
  Simplexmlwriter.print ~out xml;
  Out_channel.close out_ch
  
  

