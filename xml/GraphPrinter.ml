open Core.Std
open Printf



let output_graph (ch: out_channel) (g: SuperIndex.StringLabeledGraph.t) (_superIndex:'a) : unit = 
  let open SuperIndex in
  fprintf ch "digraph G {\n";
  StringLabeledGraph.iter_vertex (fun v -> 
    fprintf ch "\t%s;\n" v
  ) g;
  StringLabeledGraph.iter_edges (fun v1 v2 ->
    fprintf ch "\t%s -> %s;\n" v1 v2
  ) g;
  fprintf ch "}\n"


















