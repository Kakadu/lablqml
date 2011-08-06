val skipClass : Parser.clas -> bool
val skipArgument : string -> bool
exception DoSkip
exception DontSkip
val goodMeth :
  classname:string ->
  string ->
  Parser.cpptype ->
  (Parser.cpptype * 'a) list -> Parser.accessPolicy -> 'b -> bool
exception BreakS of string
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
class virtual abstractGenerator :
  Parser.indexItem Parser.Index.t ->
  object
    method cppFuncName : string -> string -> Parser.func_arg list -> string
    method private fromCamlCast :
      Parser.indexItem Parser.Index.t ->
      Parser.cpptype -> ?default:string option -> string -> castResult
    method private virtual genClass : string -> Parser.clas -> string option
    method private virtual genConstr :
      string -> out_channel -> Parser.constr -> unit
    method private virtual genEnumOfClass :
      string -> out_channel -> Parser.enum -> unit
    method private virtual genEnumOfNs :
      string -> Parser.enum -> string option
    method private virtual genMeth :
      string -> out_channel -> Parser.meth -> unit
    method genNs : string -> Parser.namespace -> unit
    method private virtual genProp :
      string -> out_channel -> Parser.prop -> unit
    method private virtual genSignal :
      string -> out_channel -> Parser.sgnl -> unit
    method private virtual genSlot :
      string -> out_channel -> Parser.slt -> unit
    method generate : Parser.namespace list -> unit
    method private index : Parser.indexItem Parser.Index.t
    method private virtual makefile : string -> string list -> unit
    method private pattern : Parser.cpptype -> pattern
    method private virtual prefix : string
    method private toCamlCast :
      Parser.cpptype -> string -> string -> castResult
    method type2str : Parser.cpptype -> string
  end
