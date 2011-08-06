class ocamlGenerator :
  string ->
  Parser.indexItem Parser.Index.t ->
  object
    method cppFuncName : string -> string -> Parser.func_arg list -> string
    method private fromCamlCast :
      Parser.indexItem Parser.Index.t ->
      Parser.cpptype ->
      ?default:string option -> string -> Generators.castResult
    method genClass : string -> Parser.clas -> string option
    method genConstr : string -> out_channel -> Parser.constr -> unit
    method genEnumOfClass : string -> out_channel -> Parser.enum -> unit
    method genEnumOfNs : string -> Parser.enum -> string option
    method genMeth : string -> out_channel -> Parser.meth -> unit
    method genNs : string -> Parser.namespace -> unit
    method genProp : string -> out_channel -> Parser.prop -> unit
    method genSignal : string -> out_channel -> Parser.sgnl -> unit
    method genSlot : string -> out_channel -> Parser.slt -> unit
    method generate : Parser.namespace list -> unit
    method private index : Parser.indexItem Parser.Index.t
    method makefile : string -> String.t list -> unit
    method private ocamlClassName : string -> string
    method private pattern : Parser.cpptype -> Generators.pattern
    method private prefix : string
    method private toCamlCast :
      Parser.cpptype -> string -> string -> Generators.castResult
    method toOcamlType : Parser.cpptype -> Generators.castResult
    method type2str : Parser.cpptype -> string
  end
