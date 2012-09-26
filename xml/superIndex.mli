
module NameKey :
  sig
    type t = string list * string
    type sexpable = t
    val sexp_of_t : 'a * Core.Core_string.t -> Sexplib.Sexp.t
    val key_of_fullname : string -> string Core.Core_list.t * string
    val t_of_sexp :
      Sexplib.Sexp.t -> string Core.Core_list.t * Core.Core_string.t
    val compare : 'a * Core.Core_string.t -> 'b * Core.Core_string.t -> int
    val make_key :
      name:String.t -> prefix:String.t list -> String.t list * String.t
    val hash : 'a * String.t -> int
    val equal : 'a * String.t -> 'b * String.t -> bool
    val to_string : t -> string
  end

module SuperIndex :
  sig
           module Key :
             sig
               type t = NameKey.t
               val sexp_of_t : t -> Sexplib.Sexp.t
               val t_of_sexp : Sexplib.Sexp.t -> t
               val compare : t -> t -> int
               type comparator = Core.Core_map.Make(NameKey).Key.comparator
               val comparator : (t, comparator) Core.Comparator.t_
             end
           type 'a t = (Key.t, 'a, Key.comparator) Core.Core_map.t
           val sexp_of_t : ('a -> Sexplib.Sexp.t) -> 'a t -> Sexplib.Sexp.t
           val t_of_sexp : (Sexplib.Sexp.t -> 'a) -> Sexplib.Sexp.t -> 'a t
           type ('a, 'b, 'c) t_ = 'b t
           type ('a, 'b, 'c) create_options =
               ('a, 'b, 'c)
               Core.Core_map_intf.create_options_without_comparator
           val empty : ('a, 'b, ('a, 'c, 'b) t_) create_options
           val singleton :
             ('a, 'b, Key.t -> 'c -> ('a, 'c, 'b) t_) create_options
           val of_alist :
             ('a, 'b,
              (Key.t * 'c) list ->
              [ `Duplicate_key of Key.t | `Ok of ('a, 'c, 'b) t_ ])
             create_options
           val of_alist_exn :
             ('a, 'b, (Key.t * 'c) list -> ('a, 'c, 'b) t_) create_options
           val of_alist_multi :
             ('a, 'b, (Key.t * 'c) list -> ('a, 'c list, 'b) t_)
             create_options
           val of_alist_fold :
             ('a, 'b,
              (Key.t * 'c) list ->
              init:'d -> f:('d -> 'c -> 'd) -> ('a, 'd, 'b) t_)
             create_options
           val of_tree :
             ('a, 'b,
              (Key.t, 'c, 'b) Core.Core_map.tree -> ('a, 'c, 'b) t_)
             create_options
           val is_empty : ('a, 'b, 'c) t_ -> bool
           val length : ('a, 'b, 'c) t_ -> int
           val add :
             ('a, 'b, 'c) t_ -> key:Key.t -> data:'b -> ('a, 'b, 'c) t_
           val add_multi :
             ('a, 'b list, 'c) t_ ->
             key:Key.t -> data:'b -> ('a, 'b list, 'c) t_
           val change :
             ('a, 'b, 'c) t_ ->
             Key.t -> ('b option -> 'b option) -> ('a, 'b, 'c) t_
           val find : ('a, 'b, 'c) t_ -> Key.t -> 'b option
           val find_exn : ('a, 'b, 'c) t_ -> Key.t -> 'b
           val remove : ('a, 'b, 'c) t_ -> Key.t -> ('a, 'b, 'c) t_
           val mem : ('a, 'b, 'c) t_ -> Key.t -> bool
           val iter :
             ('a, 'b, 'c) t_ -> f:(key:Key.t -> data:'b -> unit) -> unit
           val map : ('a, 'b, 'c) t_ -> f:('b -> 'd) -> ('a, 'd, 'c) t_
           val mapi :
             ('a, 'b, 'c) t_ ->
             f:(key:Key.t -> data:'b -> 'd) -> ('a, 'd, 'c) t_
           val fold :
             ('a, 'b, 'c) t_ ->
             init:'d -> f:(key:Key.t -> data:'b -> 'd -> 'd) -> 'd
           val fold_right :
             ('a, 'b, 'c) t_ ->
             init:'d -> f:(key:Key.t -> data:'b -> 'd -> 'd) -> 'd
           val filter :
             ('a, 'b, 'c) t_ ->
             f:(key:Key.t -> data:'b -> bool) -> ('a, 'b, 'c) t_
           val filter_map :
             ('a, 'b, 'c) t_ -> f:('b -> 'd option) -> ('a, 'd, 'c) t_
           val filter_mapi :
             ('a, 'b, 'c) t_ ->
             f:(key:Key.t -> data:'b -> 'd option) -> ('a, 'd, 'c) t_
           val compare :
             ('a -> 'a -> int) -> ('b, 'a, 'c) t_ -> ('b, 'a, 'c) t_ -> int
           val equal :
             ('a -> 'a -> bool) -> ('b, 'a, 'c) t_ -> ('b, 'a, 'c) t_ -> bool
           val keys : ('a, 'b, 'c) t_ -> Key.t list
           val data : ('a, 'b, 'c) t_ -> 'b list
           val to_alist : ('a, 'b, 'c) t_ -> (Key.t * 'b) list
           val merge :
             ('a, 'b, 'c) t_ ->
             ('a, 'd, 'c) t_ ->
             f:(key:Key.t ->
                [ `Both of 'b * 'd | `Left of 'b | `Right of 'd ] ->
                'e option) ->
             ('a, 'e, 'c) t_
           val min_elt : ('a, 'b, 'c) t_ -> (Key.t * 'b) option
           val min_elt_exn : ('a, 'b, 'c) t_ -> Key.t * 'b
           val max_elt : ('a, 'b, 'c) t_ -> (Key.t * 'b) option
           val max_elt_exn : ('a, 'b, 'c) t_ -> Key.t * 'b
           val for_all : ('a, 'b, 'c) t_ -> f:('b -> bool) -> bool
           val exists : ('a, 'b, 'c) t_ -> f:('b -> bool) -> bool
           val fold_range_inclusive :
             ('a, 'b, 'c) t_ ->
             min:Key.t ->
             max:Key.t ->
             init:'d -> f:(key:Key.t -> data:'b -> 'd -> 'd) -> 'd
           val range_to_alist :
             ('a, 'b, 'c) t_ ->
             min:Key.t -> max:Key.t -> (Key.t * 'b) list
           val prev_key : ('a, 'b, 'c) t_ -> Key.t -> (Key.t * 'b) option
           val next_key : ('a, 'b, 'c) t_ -> Key.t -> (Key.t * 'b) option
           val rank : ('a, 'b, 'c) t_ -> Key.t -> int option
           val to_tree :
             ('a, 'b, 'c) t_ -> (Key.t, 'b, 'c) Core.Core_map.tree
         end

type index_data =
    Class of Parser.clas * Parser.MethSet.t 
  | Enum of Parser.enum

type index_t  = index_data SuperIndex.t
val to_channel : index_t -> out_channel -> unit

val is_enum_exn : key:SuperIndex.Key.t -> index_t -> bool
val is_class_exn : key:SuperIndex.Key.t -> index_t -> bool

module V :
  sig
    type t = string list * string
    type sexpable = t
    val sexp_of_t : 'a * Core.Core_string.t -> Sexplib.Sexp.t
    val key_of_fullname : string -> string Core.Core_list.t * string
    val t_of_sexp :
      Sexplib.Sexp.t -> string Core.Core_list.t * Core.Core_string.t
    val compare : 'a * Core.Core_string.t -> 'b * Core.Core_string.t -> int
    val make_key :
      name:String.t -> prefix:String.t list -> String.t list * String.t
    val hash : 'a * String.t -> int
    val equal : 'a * String.t -> 'b * String.t -> bool
  end
(*
module VertexComparatorPre : sig
  type t = V.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val compare   : t  -> t -> int  
end
module VertexComparator : sig
  type t = VertexComparatorPre.t
  val sexp_of_t : t -> Sexplib.Sexp.t
  val t_of_sexp : Sexplib.Sexp.t -> t
  val compare   : t  -> t -> int  
end
  *)
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
(*    val kill_and_fall : t -> vertex -> unit *)
  end

val build_graph      : Parser.namespace -> (index_data SuperIndex.t ref) * G.t
val build_superindex : Parser.namespace  -> index_data SuperIndex.t * G.t * SuperIndex.Key.t Core.Core_queue.t

