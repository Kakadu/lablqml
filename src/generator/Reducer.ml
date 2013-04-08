open Printf
open Graph

module type G = sig
  include Graph.Components.G
  module E : sig
    type t
  end
  val iter_edges: (V.t -> V.t -> unit) -> t -> unit
  val add_edge: t  -> V.t -> V.t -> unit
  val create: ?size:int -> unit -> t
end

module Make(G1: G)(Gout: G) = struct
  let rebuild (gin: G1.t) ~(f_alone:G1.V.t -> Gout.V.t) ~(f_cycle:G1.V.t list -> Gout.V.t) : Gout.t =
    let module T = Graph.Components.Make(G1) in
    let module H = Hashtbl.Make(G1.V) in
    let vertex_map = H.create 999 in
    let components = T.scc_list gin in
    let gout = Gout.create () in
    List.iter (function
      | [] -> assert false
      | [x] ->
          let new_v = f_alone x in
          H.add vertex_map x new_v
      | xs ->
          let new_v = f_cycle xs in
          List.iter (fun x -> H.add vertex_map x new_v) xs
    ) components;
    G1.iter_edges (fun a b ->
      Gout.add_edge gout (H.find vertex_map a) (H.find vertex_map b)
    ) gin;
    gout
end
