open Parser

val skipClass : clas -> bool
val skipArgument : cpptype -> bool
exception DoSkip
exception DontSkip

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
    InvalidPattern
  | PrimitivePattern
  | ObjectPattern
  | EnumPattern

val cpp_func_name : classname:string -> methname:string -> func_arg list -> string
val is_good_meth : classname:string -> meth -> bool


val pattern : SuperIndex.index_t -> cpptype -> pattern
val is_abstract_class : prefix:string list -> SuperIndex.index_t -> string -> bool

class virtual abstractGenerator :
  SuperIndex.index_t ->
  object
    method private fromCamlCast :
      SuperIndex.index_t ->
      Parser.cpptype -> ?default:string option -> string -> castResult
    method private virtual gen_class : prefix:string list -> dir:string -> Parser.clas -> string option

    method private virtual genConstr :
      string -> out_channel -> Parser.constr -> unit
    method private virtual gen_enumOfClass :
      string -> out_channel -> Parser.enum -> unit
    method private virtual gen_enumOfNs :
      prefix:string list -> dir:string -> Parser.enum -> string option
    method private virtual genMeth :
      string -> out_channel -> Parser.meth -> unit
    method gen_ns : prefix:string list -> dir:string -> Parser.namespace -> unit
    method private virtual genProp :
      string -> out_channel -> Parser.prop -> unit
    method private virtual genSignal :
      string -> out_channel -> Parser.sgnl -> unit
    method private virtual genSlot :
      string -> out_channel -> Parser.slt -> unit
    method generate : namespace -> unit
    method private index : SuperIndex.index_t
    method private virtual makefile : string -> string list -> unit
    method private virtual prefix : string
    method private toCamlCast :
      Parser.cpptype -> string -> string -> castResult
  end
