open Core.Std

val (|>) : 'a -> ('a -> 'b) -> 'b

val filter_enum_members :
  (String.t * String.t option) list -> string list
val startswith : prefix:String.t -> string -> bool
val make_out_name : classname:string -> string -> string
val skip_meth : classname:string -> String.t -> bool
val endswith : postfix:String.t -> String.t -> bool
val string_split : on:String.t -> String.t -> String.t List.t
type cpptype = {
  t_name : string;
  t_is_const : bool;
  t_indirections : int;
  t_is_ref : bool;
  t_params : cpptype list;
}
and func_arg = {
  arg_type : cpptype;
  arg_name : string option;
  arg_default : string option;
}
and meth = {
  m_res : cpptype;
  m_name : string;
  m_args : func_arg list;
  m_declared : string;
  m_out_name : string;
  m_access : [ `Private | `Protected | `Public ];
  m_modif : [ `Abstract | `Normal | `Static ];
}
val cpptype_of_sexp__ : Sexplib.Sexp.t -> cpptype
val cpptype_of_sexp : Sexplib.Sexp.t -> cpptype
val func_arg_of_sexp__ : Sexplib.Sexp.t -> func_arg
val func_arg_of_sexp : Sexplib.Sexp.t -> func_arg
val meth_of_sexp__ : Sexplib.Sexp.t -> meth
val meth_of_sexp : Sexplib.Sexp.t -> meth
val sexp_of_cpptype : cpptype -> Sexplib.Sexp.t
val sexp_of_func_arg : func_arg -> Sexplib.Sexp.t
val sexp_of_meth : meth -> Sexplib.Sexp.t
val simple_arg : cpptype -> func_arg
val is_public : [< `Private | `Protected | `Public ] -> bool
val is_virtual : [< `Abstract | `Static | `Virtual ] -> bool
val void_type : cpptype
val remove_defaults : meth -> meth
val unreference : cpptype -> cpptype
exception Ans of int
val wrap : int -> unit
val compare_cpptype : cpptype -> cpptype -> int
val compare_meth : meth -> meth -> int
module MethKey :
  sig
    type t = meth
    type sexpable = meth
    val sexp_of_t : meth -> Sexplib.Sexp.t
    val t_of_sexp : Sexplib.Sexp.t -> meth
    val compare : meth -> meth -> int
  end
module MethSet :
  sig
    module Elt :
      sig
        type t = MethKey.t
        val sexp_of_t : t -> Sexplib.Sexp.t
        val t_of_sexp : Sexplib.Sexp.t -> t
        val compare : t -> t -> int
        type comparator = Core.Core_set.Make(MethKey).Elt.comparator
        val comparator : (t, comparator) Core.Comparator.t_
      end
    type t = (Elt.t, Elt.comparator) Core.Core_set.t
    val sexp_of_t : t -> Sexplib.Sexp.t
    val t_of_sexp : Sexplib.Sexp.t -> t
    type ('a, 'b) t_ = t
    type ('a, 'b) tree = ('a, 'b) Core.Core_set.Make(MethKey).tree
    
    val empty :
      ('a, 'b, ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val union_list :
      ('a, 'b, ('a, 'b) t_ list -> ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val of_list :
      ('a, 'b, Elt.t list -> ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val of_array :
      ('a, 'b, Elt.t array -> ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val stable_dedup_list :
      ('a, 'b, Elt.t list -> Elt.t list)
      Core.Core_set_intf.create_options_without_comparator
    val filter_map :
      ('a, 'b,
       ('c, 'd) Core.Core_set.t -> f:('c -> Elt.t option) -> ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val of_tree :
      ('a, 'b, (Elt.t, 'b) tree -> ('a, 'b) t_)
      Core.Core_set_intf.create_options_without_comparator
    val length : ('a, 'b) t_ -> int
    val is_empty : ('a, 'b) t_ -> bool
    val iter : ('a, 'b) t_ -> f:(Elt.t -> unit) -> unit
    val fold : ('a, 'b) t_ -> init:'c -> f:('c -> Elt.t -> 'c) -> 'c
    val exists : ('a, 'b) t_ -> f:(Elt.t -> bool) -> bool
    val for_all : ('a, 'b) t_ -> f:(Elt.t -> bool) -> bool
    val count : ('a, 'b) t_ -> f:(Elt.t -> bool) -> int
    val find : ('a, 'b) t_ -> f:(Elt.t -> bool) -> Elt.t option
    val find_map : ('a, 'b) t_ -> f:(Elt.t -> 'c option) -> 'c option
    val to_list : ('a, 'b) t_ -> Elt.t list
    val to_array : ('a, 'b) t_ -> Elt.t array
    val mem : ('a, 'b) t_ -> Elt.t -> bool
    val add : ('a, 'b) t_ -> Elt.t -> ('a, 'b) t_
    val remove : ('a, 'b) t_ -> Elt.t -> ('a, 'b) t_
    val union : ('a, 'b) t_ -> ('a, 'b) t_ -> ('a, 'b) t_
    val inter : ('a, 'b) t_ -> ('a, 'b) t_ -> ('a, 'b) t_
    val diff : ('a, 'b) t_ -> ('a, 'b) t_ -> ('a, 'b) t_
    val compare : ('a, 'b) t_ -> ('a, 'b) t_ -> int
    val equal : ('a, 'b) t_ -> ('a, 'b) t_ -> bool
    val subset : ('a, 'b) t_ -> ('a, 'b) t_ -> bool
    val fold_until :
      ('a, 'b) t_ ->
      init:'c -> f:('c -> Elt.t -> [ `Continue of 'c | `Stop of 'c ]) -> 'c
    val fold_right : ('a, 'b) t_ -> init:'c -> f:(Elt.t -> 'c -> 'c) -> 'c
    val filter : ('a, 'b) t_ -> f:(Elt.t -> bool) -> ('a, 'b) t_
    val partition_tf :
      ('a, 'b) t_ -> f:(Elt.t -> bool) -> ('a, 'b) t_ * ('a, 'b) t_
    val elements : ('a, 'b) t_ -> Elt.t list
    val min_elt : ('a, 'b) t_ -> Elt.t option
    val min_elt_exn : ('a, 'b) t_ -> Elt.t
    val max_elt : ('a, 'b) t_ -> Elt.t option
    val max_elt_exn : ('a, 'b) t_ -> Elt.t
    val choose : ('a, 'b) t_ -> Elt.t option
    val choose_exn : ('a, 'b) t_ -> Elt.t
    val split : ('a, 'b) t_ -> Elt.t -> ('a, 'b) t_ * bool * ('a, 'b) t_
    val group_by :
      ('a, 'b) t_ -> equiv:(Elt.t -> Elt.t -> bool) -> ('a, 'b) t_ list
    val find_exn : ('a, 'b) t_ -> f:(Elt.t -> bool) -> Elt.t
    val find_index : ('a, 'b) t_ -> int -> Elt.t option
    val remove_index : ('a, 'b) t_ -> int -> ('a, 'b) t_
    val to_tree : ('a, 'b) t_ -> (Elt.t, 'b) tree
    val compare_items : meth -> meth -> int
    val map :
      f:(Elt.t -> Elt.t) ->
      ('a, 'c) t_ ->
      ('d, 'e, ('d, 'e) t_)
      Core.Core_set_intf.create_options_without_comparator
    val remove_set : base:('a, 'b) t_ -> ('c, 'd) t_ -> ('a, 'b) t_
  end
type enum = {
  e_flag_name : string;
  e_name : string;
  e_items : string list;
  e_access : [ `Private | `Protected | `Public ];
}
val enum_of_sexp__ : Sexplib.Sexp.t -> enum
val enum_of_sexp : Sexplib.Sexp.t -> enum
val sexp_of_enum : enum -> Sexplib.Sexp.t
type constr = func_arg list
val constr_of_sexp__ : Sexplib.Sexp.t -> func_arg list
val constr_of_sexp : Sexplib.Sexp.t -> func_arg list
val sexp_of_constr : func_arg list -> Sexplib.Sexp.t
type prop = string * string option * string option
val prop_of_sexp__ : Sexplib.Sexp.t -> string * string option * string option
val prop_of_sexp : Sexplib.Sexp.t -> string * string option * string option
val sexp_of_prop : string * string option * string option -> Sexplib.Sexp.t
type sgnl = string * func_arg list
val sgnl_of_sexp__ : Sexplib.Sexp.t -> string * func_arg list
val sgnl_of_sexp : Sexplib.Sexp.t -> string * func_arg list
val sexp_of_sgnl : string * func_arg list -> Sexplib.Sexp.t
type clas = {
  c_inherits : string list;
  c_props : prop list;
  c_sigs : sgnl list;
  c_slots : MethSet.t;
  c_meths : MethSet.t;
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
and slt = meth
val clas_of_sexp__ : Sexplib.Sexp.t -> clas
val clas_of_sexp : Sexplib.Sexp.t -> clas
val namespace_of_sexp__ : Sexplib.Sexp.t -> namespace
val namespace_of_sexp : Sexplib.Sexp.t -> namespace
val slt_of_sexp__ : Sexplib.Sexp.t -> meth
val slt_of_sexp : Sexplib.Sexp.t -> meth
val sexp_of_clas : clas -> Sexplib.Sexp.t
val sexp_of_namespace : namespace -> Sexplib.Sexp.t
val sexp_of_slt : meth -> Sexplib.Sexp.t
val empty_namespace : namespace
val ptrtype_of_class : clas -> cpptype
val ptrtype_of_classname : string -> cpptype
val is_void_type : cpptype -> bool
val meth_of_constr : classname:string -> func_arg list -> meth
val string_of_type : cpptype -> String.t
val string_of_arg : func_arg -> String.t
val string_of_meth : meth -> string
val string_of_constr : classname:string -> func_arg list -> string
val headl : int -> 'a list -> 'a List.t * 'a list
val skipNs : string -> bool
val fixTemplateClassName : string -> string
val str_replace : patt:(string * string) List.t -> string -> string
exception Break
val strip_dd : prefix:String.t -> String.t -> String.t
val qtFlags : (string * string) list
val fixEnumName : string -> string
val str2policy : string -> [> `Private | `Protected | `Public ]
val parse_prop :
  string * (string * 'a) list * Simplexmlparser.xml List.t ->
  'a * string option * string option
val parse_arg :
  string * (string, string) List.Assoc.t * Simplexmlparser.xml list ->
  func_arg
val parse_enum :
  String.t ->
  string * (string, String.t) List.Assoc.t * Simplexmlparser.xml List.t ->
  enum option
val build : Simplexmlparser.xml -> namespace
val parse_class :
  String.t ->
  string * (string, String.t) List.Assoc.t * Simplexmlparser.xml List.t ->
  clas
val parse_ns :
  String.t ->
  string * (string * String.t) list * Simplexmlparser.xml List.t -> namespace
