type default = [ `Int ]
type model   = [ `Model ]
type cppobj  = [ `Cppobj ]

type _ argt =
  | Int        : [> `Int ] argt
  | Model      : [> `Model   ] argt
  | Model2     : [> `Model   ] argt
  | Cppobj     : [> `Cppobj  ] argt
  | List       : 'a argt -> 'a argt

let f : [ default | model ] argt -> unit = fun _ -> ()
let model : model argt = Model
(*
let () = f model (* doesn't compile (no variance) *)
*)

let rec (model_plus_default : model argt -> [ default | model ] argt) = function
  | (Model as y)
  | (Model2 as y) -> y
  | List x -> List (model_plus_default x)

let () = f (model_plus_default model)

let f : [ default | model ] argt -> unit  = function
  | Int
  | Model
  | Model2
  | List _     -> ()
(*
let rec length (type a) (x: a argt) : int = match x with
| Int -> 0
| Model -> 1
| Cppobj -> 2
| List x -> length x  (* ERROR: The type constructor a would escape its scope *)
*)

type any = [ `Cppobj | `Int | `Model ]

let rec length : any argt -> int = function
| Int
| Model
| Model2 -> 1
| Cppobj -> 2
| List x -> length x

let aux_variables_count  =
  let rec helper : any argt -> int = function
    | Int -> 0
    | Model
    | Model2 -> 1
    | Cppobj -> failwith "not implemented"
    | List x -> helper x + 2
  in
  helper


(* doesn't compile *)
(*let _ = f Int  *)

(* doesn't compile too because of invariance *)
(*let _ = f (Int :> [ default | model ] argt)*)

let add_model_to_default: default argt -> [ model | default ] argt = function
(Int as x) -> x

(*let add2  = function
(Int as x) -> x*)


module M : sig
  type default = [ `Default ]
  type mode = [ `Model ]
  type cppobj = [ `Cppobj ]
  type +'a t = private Int | Model | Obj

  val obj : cppobj t
  val model : model t
  val int : default t

  val foo : [ default | model ] t -> int

end = struct

  type default = [ `Default ]
  type mode = [ `Model ]
  type cppobj = [ `Cppobj ]

  type 'a t = Int | Model | Obj

  let int : default t = Int
  let model = Model
  let obj = Obj

  let foo _ = 1

end


let _ : int = M.(foo (model :> [default | model ]  t))

(*
module M3 : sig
  type default = [ `Default ]
  type model   = [ `Model ]
  type cppobj  = [ `Cppobj ]
  type any = [ default | model | cppobj ]

  type 'a argt =
    | Int        : default argt
    | ModelIndex : model argt
    | CppObj     : cppobj argt

end = struct
  type default = [ `Default ]
  type model   = [ `Model ]
  type cppobj  = [ `Cppobj ]
  type any = [ default | model | cppobj ]

  type ('from, 'dest) upcast = Cast
  let cast : ('from, 'dest) upcast -> 'from -> 'dest = Obj.magic

  module Cast = struct
    type ('a, 'b) model_t = ('a, 'b) upcast
      constraint 'b = [> ]
      constraint 'a = [ `model | 'b ]

    let add_model  ('a, [ model  | 'a ]) upcast = Cast
    let add_cppobj : ('a, [ cppobj | 'a ]) upcast = Cast
  end

  type 'a argt =
    | Int        : default argt
    | ModelIndex : model argt
    | CppObj     : cppobj argt



end

*)



type (_,_) eql = Refl : ('a, 'a) eql

(*module M4 = struct
  type +_ argt =
    | Int        : ([`Default], 'a) eql -> 'a argt
    | ModelIndex : ([`Model],   'a) eql -> 'a argt
    | CppObj     : ([`Cppobj],  'a) eql -> 'a argt

end*)


module M5 = struct

  type bot

  (*type (+_,+_,+_) t =
    | Fst : ('a, bot, bot) t
    | Snd : (bot, 'a, bot) t
    | Trd : (bot, bot, 'a) t*)
    (* In this GADT definition, the variance of some parameter cannot be checked
     *)


end
