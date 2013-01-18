open Core.Std

let (|>) = Core.Fn.(|!)

let legend = String.concat ~sep:"\\n" 
  [ "Diamond nodes are classes which are described in thne input XML file"
  
  ]
(*
let fix_name v = v 
        |> Str.global_replace (Str.regexp "::") "_dd_"
        |> Str.global_replace (Str.regexp "<") "_lt_"
        |> Str.global_replace (Str.regexp ">") "_md_"
        |> Str.global_replace (Str.regexp ",") "_zpt_"
        |> Str.global_replace (Str.regexp "*") "_star_"
        |> Str.global_replace (Str.regexp "&amp;") "_AMP_"
          *)

let output_graph (ch: out_channel) (g: SuperIndex.StringLabeledGraph.t) ~is_declared : unit = 
  let printf fmt = Printf.fprintf ch fmt in
  let open SuperIndex in
  printf "digraph G {\n";
  StringLabeledGraph.iter_vertex (fun v -> 
    printf "\t\"%s\"[shape=%s];\n" v (if is_declared v then "diamond" else "ellipse")
  ) g;
  StringLabeledGraph.iter_edges (fun v1 v2 ->
    printf "\t\"%s\" -> \"%s\";\n" v1 v2
  ) g;

  printf "\tlegend [shape=box,label=\"%s\"];\n" legend;
  printf "}\n"


















