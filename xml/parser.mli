type accessPolicy = Public | Private | Protected
type modifiers = Static | Abstract | Virtual
val ( |> ) : 'a -> ('a -> 'b) -> 'b
val ( $ ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
type clas = {
  c_inherits : string list;
  c_props : prop list;
  c_sigs : sgnl list;
  c_slots : slt list;
  c_meths : meth list;
  c_enums : enum list;
  c_constrs : constr list;
  c_name : string;
}
and namespace = {
  ns_name : string;
  ns_classes : clas list;
  ns_enums : enum list;
  ns_ns : namespace list;
}
and enum = string * string list
and cpptype = {
  t_name : string;
  t_is_const : bool;
  t_indirections : int;
  t_is_ref : bool;
  t_params : cpptype list;
}
and func_arg = cpptype * string option
and meth = string * func_arg list * cpptype * accessPolicy * modifiers list
and constr = func_arg list * accessPolicy * modifiers list
and slt = string * func_arg list
and sgnl = string * func_arg list
and prop = string * string option * string option

val type2str: cpptype -> string
val meth2str: meth -> string

val isAbstractMeth : 'a * 'b * 'c * 'd * modifiers list -> bool
val unreference : cpptype -> cpptype
val skipNs : string -> bool
val isTemplateClass : string -> bool
val isInnerClass : string -> bool
val fixTemplateClassName : string -> string
val str_replace : patt:(string * string) list -> string -> string
val startswith : prefix:string -> string -> bool
val endswith : postfix:string -> string -> bool
val skipMeth : classname:string -> string -> bool
exception Break
val strip_dd : prefix:string -> string -> string
val attrib_opt : 'a -> ('a * 'b) list -> 'b option
val xname : Simplexmlparser.xml -> string
val attrib : 'a -> ('a * 'b) list -> 'b
val build : Simplexmlparser.xml -> namespace
val parse_arg :
  string * (string * string) list * Simplexmlparser.xml list -> func_arg
val parse_class :
  string ->
  string * (string * string) list * Simplexmlparser.xml list -> clas
val parse_enum :
  string ->
  string * (string * string) list * Simplexmlparser.xml list -> enum
val parse_prop :
  string * (string * string) list * Simplexmlparser.xml list -> prop
val parse_ns :
  string ->
  string * (string * string) list * Simplexmlparser.xml list -> namespace


(** The same as fun n list -> (Core_list.take n list, Core_list.drop n list) *)
val headl: int -> 'a list -> 'a list * 'a list

type short_method = string * cpptype list
type indexItem = | Ns of namespace 
		 | Class of clas * short_method list (* clas and its pure virtual members *)
		 | Enum of enum
exception NotInIndex of string
module Index :
  sig
    type key = Core.Core_string.t
    type 'a t = 'a Core.Core_map.Make(Core.Core_string).t
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
    val print_keys : 'a t -> unit
    val isEnum : key -> indexItem t -> bool
    val isClass : key -> indexItem t -> bool
  end
(*
type index_t = indexItem Index.t
val build_index : namespace list -> indexItem Index.t
val checkBasesExists : indexItem Index.t -> bool
module StringSet :
  sig
    type elt = Core.Core_string.t
    type t = Core.Core_set.Make(Core.Core_string).t
    val empty : t
    val is_empty : t -> bool
    val mem : t -> elt -> bool
    val add : t -> elt -> t
    val singleton : elt -> t
    val remove : t -> elt -> t 
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : f:(elt -> unit) -> t -> unit
    val fold : f:(elt -> 'a -> 'a) -> t -> init:'a -> 'a
    val for_all : f:(elt -> bool) -> t -> bool
    val exists : f:(elt -> bool) -> t -> bool
    val filter : f:(elt -> bool) -> t -> t
    val partition : f:(elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt option
    val max_elt : t -> elt option
    val choose : t -> elt option
    val split : elt -> t -> t * bool * t
  end
val postBuildIndex : indexItem Index.t -> indexItem Index.t

val removeClassesWithoutBases  : indexItem Index.t -> indexItem Index.t
  *)
