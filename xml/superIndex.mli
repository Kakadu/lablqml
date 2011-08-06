
module NameKey :
  sig
    type t = string list * string
    type sexpable = t
    val sexp_of_t : 'a * Core.Core_string.sexpable -> Sexplib.Sexp.t
    val key_of_fullname : string -> string Core.Core_list.t * string
    val t_of_sexp :
      Sexplib.Sexp.t -> string Core.Core_list.t * Core.Core_string.sexpable
    val compare : 'a * Core.Core_string.t -> 'b * Core.Core_string.t -> int
    val make_key :
      name:String.t -> prefix:String.t list -> String.t list * String.t
    val hash : 'a * String.t -> int
    val equal : 'a * String.t -> 'b * String.t -> bool
  end

module SuperIndex :
  sig
    type key = NameKey.t
    type 'a t = 'a Core.Core_map.Make(NameKey).t
(*    module T : sig type 'a key = key type ('a, 'b) t = 'b t end *)
    type 'a sexpable = 'a t
    val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a sexpable -> Sexplib.Sexp.t
    val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a sexpable
    val empty : 'a t
    val singleton : key -> 'a -> 'a t
    val is_empty : 'a t -> bool
    val cardinal : 'a t -> int
    val add : key:key -> data:'a -> 'a t -> 'a t
    val find_exn : 'a t -> key -> 'a
    val find : 'a t -> key -> 'a option
    val remove : 'a t -> key -> 'a t
    val mem : 'a t -> key -> bool
    val iter : f:(key:key -> data:'a -> unit) -> 'a t -> unit
    val map : f:('a -> 'b) -> 'a t -> 'b t
    val mapi : f:(key:key -> data:'a -> 'b) -> 'a t -> 'b t
    val fold : f:(key:key -> data:'a -> 'b -> 'b) -> 'a t -> init:'b -> 'b
    val filter : f:(key:key -> data:'a -> bool) -> 'a t -> 'a t
    val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
    val filter_mapi : f:(key:key -> data:'a -> 'b option) -> 'a t -> 'b t
    val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
    val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
    val keys : 'a t -> key list
    val has_key : 'a t -> key -> bool
    val data : 'a t -> 'a list
    val of_alist : (key * 'a) list -> [ `Duplicate_key of key | `Ok of 'a t ]
    val of_alist_exn : (key * 'a) list -> 'a t
    val of_alist_multi : (key * 'a) list -> 'a list t
    val to_alist : 'a t -> (key * 'a) list
    val combine_alist :
      (key * 'a) list -> init:'b -> f:('a -> 'b -> 'b) -> 'b t
    val merge :
      f:(key:key -> 'a option -> 'b option -> 'c option) ->
      'a t -> 'b t -> 'c t
    val min_elt : 'a t -> (key * 'a) option
    val min_elt_exn : 'a t -> key * 'a
    val max_elt : 'a t -> (key * 'a) option
    val max_elt_exn : 'a t -> key * 'a
    val for_all : f:('a -> bool) -> 'a t -> bool
    val exists : f:('a -> bool) -> 'a t -> bool
  end
type index_data =
    Class of Parser.clas * Parser.MethSet.t
  | Enum of Parser.enum

module V :
  sig
    type t = string list * string
    type sexpable = t
    val sexp_of_t : 'a * Core.Core_string.sexpable -> Sexplib.Sexp.t
    val key_of_fullname : string -> string Core.Core_list.t * string
    val t_of_sexp :
      Sexplib.Sexp.t -> string Core.Core_list.t * Core.Core_string.sexpable
    val compare : 'a * Core.Core_string.t -> 'b * Core.Core_string.t -> int
    val make_key :
      name:String.t -> prefix:String.t list -> String.t list * String.t
    val hash : 'a * String.t -> int
    val equal : 'a * String.t -> 'b * String.t -> bool
  end

module G :
  sig
    type t = Graph.Imperative.Digraph.Concrete(V).t
    module V :
      sig
        type t = V.t
        val compare : t -> t -> int
        val hash : t -> int
        val equal : t -> t -> bool
        type label = V.t
        val create : label -> t
        val label : t -> label
      end
    type vertex = V.t
    module E :
      sig
        type t = V.t * V.t
        val compare : t -> t -> int
        type vertex = V.t
        val src : t -> vertex
        val dst : t -> vertex
        type label = unit
        val create : vertex -> label -> vertex -> t
        val label : t -> label
      end
    type edge = E.t
    val is_directed : bool
    val is_empty : t -> bool
    val nb_vertex : t -> int
    val nb_edges : t -> int
    val out_degree : t -> vertex -> int
    val in_degree : t -> vertex -> int
    val mem_vertex : t -> vertex -> bool
    val mem_edge : t -> vertex -> vertex -> bool
    val mem_edge_e : t -> edge -> bool
    val find_edge : t -> vertex -> vertex -> edge
    val succ : t -> vertex -> vertex list
    val pred : t -> vertex -> vertex list
    val succ_e : t -> vertex -> edge list
    val pred_e : t -> vertex -> edge list
    val iter_vertex : (vertex -> unit) -> t -> unit
    val fold_vertex : (vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges : (vertex -> vertex -> unit) -> t -> unit
    val fold_edges : (vertex -> vertex -> 'a -> 'a) -> t -> 'a -> 'a
    val iter_edges_e : (edge -> unit) -> t -> unit
    val fold_edges_e : (edge -> 'a -> 'a) -> t -> 'a -> 'a
    val map_vertex : (vertex -> vertex) -> t -> t
    val iter_succ : (vertex -> unit) -> t -> vertex -> unit
    val iter_pred : (vertex -> unit) -> t -> vertex -> unit
    val fold_succ : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val fold_pred : (vertex -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_succ_e : (edge -> unit) -> t -> vertex -> unit
    val fold_succ_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val iter_pred_e : (edge -> unit) -> t -> vertex -> unit
    val fold_pred_e : (edge -> 'a -> 'a) -> t -> vertex -> 'a -> 'a
    val create : ?size:int -> unit -> t
    val clear : t -> unit
    val copy : t -> t
    val add_vertex : t -> vertex -> unit
    val remove_vertex : t -> vertex -> unit
    val add_edge : t -> vertex -> vertex -> unit
    val add_edge_e : t -> edge -> unit
    val remove_edge : t -> vertex -> vertex -> unit
    val remove_edge_e : t -> edge -> unit
    val graph_attributes : t -> 'a list
    val default_vertex_attributes : 'a -> 'b list
    val vertex_name : V.t -> string
    val vertex_attributes : V.t -> [> `Label of string ] list
    val get_subgraph : 'a -> 'b option
    val default_edge_attributes : 'a -> 'b list
    val edge_attributes : 'a -> 'b list
    val kill_and_fall : t -> vertex -> unit
  end

val build_superindex : Parser.namespace  -> index_data SuperIndex.t * G.t
