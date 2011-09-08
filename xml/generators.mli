open Parser

val skipClass : prefix:string list -> clas -> bool
val skipArgument : index:SuperIndex.index_t -> func_arg -> bool
exception DoSkip
exception DontSkip
exception BreakSilent
exception BreakS of string
val breaks : string -> 'a

type t1 = string
and t2 = string
and castResult =
    Success of t1
  | CastError of t2
  | CastValueType of t2
  | CastTemplate of t2
exception BreakOk of t1
exception BreakFail of t2
exception BreakResult of castResult
type pattern =
  | InvalidPattern
  | PrimitivePattern
  | ObjectPattern
  | EnumPattern  of enum * SuperIndex.NameKey.t
  | ObjectDefaultPattern

val cpp_func_name : classname:string -> methname:string -> func_arg list -> string
val is_good_meth : classname:string -> index:SuperIndex.index_t -> meth -> bool

val enum_conv_func_names : (string list * string) -> string*string
val pattern : SuperIndex.index_t -> Parser.func_arg -> pattern
val is_abstract_class : prefix:string list -> SuperIndex.index_t -> string -> bool

class virtual abstractGenerator :
  SuperIndex.index_t ->
  object
    method private toCamlCast :
      ?forcePattern: pattern option -> Parser.func_arg -> string -> string -> castResult
    method private fromCamlCast :
      SuperIndex.index_t ->
      Parser.cpptype -> default:string option -> string -> castResult
    method private virtual gen_class : prefix:string list -> dir:string -> Parser.clas -> string option

    method private virtual genConstr :
      prefix:string list -> string -> out_channel -> Parser.constr -> unit
(*    method private virtual gen_enumOfClass :
	string -> out_channel -> Parser.enum -> unit *)
    method private virtual gen_enum_in_ns :
      key:SuperIndex.NameKey.t -> dir:string -> Parser.enum -> string option
    method private virtual genMeth :
      prefix:string list -> string -> out_channel -> Parser.meth -> unit
(*    method gen_ns : prefix:string list -> dir:string -> Parser.namespace -> unit *)
    method private virtual genProp :
      string -> out_channel -> Parser.prop -> unit
(*    method generate : namespace -> unit *)
    method private index : SuperIndex.index_t
    method private virtual makefile : string -> string list -> unit
    method private virtual prefix : string
  end
