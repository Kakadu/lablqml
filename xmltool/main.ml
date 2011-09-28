open Core
module List = Core_list
module Set = Core_set
open SuperIndex
open Parser
open Printf

let names = ref Set.empty
let xml_name = ref "../aaa.xml";;
let out_name = ref "out.xml";;

let () = Arg.parse [ 
  ("-in",  Arg.String (fun s -> xml_name := s), "input xml file");
  ("-out", Arg.String (fun s -> out_name := s), "output xml file")
 ]
  (fun name -> names := Set.add !names name) "haha"

let root = Simplexmlparser.xmlparser_file !xml_name |> List.hd_exn
module Printer = XML_print.MakeSimple(XML)(struct let emptytags=[] end) 

let (_,g) = build_graph (root |> Parser.build)

open Simplexmlparser
let () = 
  let start_set = ref Set.empty in
  G.iter_vertex (fun v ->
    let name = G.vertex_name v in
    if Set.mem !names name then start_set:= Set.add !start_set v
  ) g;
  let ans_vs = ref !start_set in
  let rec loop set = 
    if Set.is_empty set then () else begin
      let parents = ref Set.empty in
      Set.iter set ~f:(G.iter_pred (fun  v -> parents := Set.add !parents v ) g);
      ans_vs:= Set.union !ans_vs !parents;
      loop !parents
    end
  in
  loop !start_set;

(*  Set.iter (fun x -> print_endline (G.vertex_name x)) !ans_vs; *)
  let names = Set.union (Set.map ~f:G.vertex_name !ans_vs) !names in
  
  let lst = match root with
    | Element ("code",_,lst) -> lst
    | _ -> assert false
  in
  let ans_lst = List.filter lst ~f:(fun e -> match e with
    | Element (_,attr,_) ->
      let name = List.Assoc.find_exn attr "name" in
      Set.mem names name
    | _ -> false
  ) in
  let xml = Element ("code",[],ans_lst) in
  let out_ch = open_out !out_name in
  printf "Writing out xml....\n";
  let out s = Printf.fprintf out_ch "%s" s in
  Simplexmlwriter.print ~out xml;
  close_out out_ch
  
  

