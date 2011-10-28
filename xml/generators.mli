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

val sexp_of_pattern : pattern -> Sexplib.Sexp.t 
val cpp_stub_name: classname:string -> ?res_n_name:cpptype*string -> ?is_byte:bool 
  ->  [< `Private | `Protected | `Public ] -> func_arg list -> string
val is_good_meth : classname:string -> index:SuperIndex.index_t -> meth -> bool

val enum_conv_func_names : (string list * string) -> string*string
val pattern : SuperIndex.index_t -> Parser.func_arg -> pattern
val is_abstract_class : prefix:string list -> SuperIndex.index_t -> string -> bool

class virtual abstractGenerator : SuperIndex.G.t -> SuperIndex.index_t ->
  object
    method private toCamlCast :
      ?forcePattern: pattern option -> Parser.func_arg -> string -> string -> castResult
    method private fromCamlCast :
      SuperIndex.index_t ->
      Parser.func_arg ->  ?cpp_argname:string option  -> string -> castResult
    method private index : SuperIndex.index_t
    method private virtual prefix : string
    method isQObject: SuperIndex.NameKey.t -> bool
  end

