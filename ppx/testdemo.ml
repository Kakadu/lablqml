open Ppxlib

let dummy_loc = Location.in_file "dummy.ml"

type stream_item = (arg_label * expression)
type 'a parse_result = Failed | Parsed of 'a * stream_item list
type 'a parser = stream_item list -> 'a parse_result

let myparse p onOK xs =
  Ast_pattern.parse p dummy_loc xs ~on_error:(fun () -> Failed) onOK

let args_end : unit parser = fun xs ->
(*  Format.printf "calling args_end %s %d\n%!" __FILE__ __LINE__;*)
  Ast_pattern.parse Ast_pattern.nil dummy_loc xs ~on_error:(fun () -> Failed)
    (Parsed ((), []))

let singleton : unit parser = fun xs ->
  let open Ast_pattern in
  let p = (no_label @@ pexp_ident (lident (string "singleton"))) ^:: __ in
  parse p dummy_loc xs ~on_error:(fun () ->
    Failed)
    (fun tl -> Parsed ((), tl))

let name : string parser =
  let open Ast_pattern in
  let p = (pair (labelled (string "name")) (pexp_constant @@ pconst_string __ none)) ^:: __ in
  myparse p (fun name tl -> Parsed (name, tl))

let (>>=): 'a parser -> ('a -> 'b parser) -> 'b parser = fun  p f xs ->
  match p xs with
  | Failed -> Failed
  | Parsed (x, tl) -> f x tl

let return x xs = Parsed (x, xs)

let many : 'a parser -> 'a list parser = fun p ->
  let rec helper acc input =
    match p input with
    | Failed -> return (List.rev acc) input
    | Parsed (x,tl) -> helper (x::acc) tl
  in
  helper []


(*let ( *> ) p q xs = p xs >>= fun _ -> q*)
let ( >> ) : 'a parser -> 'b parser -> 'b parser = fun p q -> p >>= fun _ -> q
let (<|>) p q xs =
  match p xs with
  | Failed -> q xs
  | r -> r

let choice ps input =
  let rec helper = function
  | [] -> Failed
  | h::ps ->
      match h input with
      | Failed ->
(*          Format.printf "choiced parser failed: %s %d\n%!" __FILE__ __LINE__;*)
          helper ps
      | Parsed (r,tl) -> Parsed (r,tl)
  in
  helper ps

type info =
  { mutable is_singleton: bool
  ; mutable name: string option
  } [@@deriving show]



let all : info parser = fun xs ->
  let rez = { is_singleton = false; name = None } in
  xs |>
  ((many @@ choice
    [
      (singleton >>= fun () -> rez.is_singleton <- true; return ())
    ; (name      >>= fun s -> rez.name <- Some s; return ())
    ])
  >> args_end >> (return rez))

let qmlargs : expression -> unit parse_result = fun e ->
  let p =
    let open Ppxlib.Ast_pattern in
    pexp_apply
      (pexp_ident (lident (string "qml"))) __
  in
  Ast_pattern.parse p dummy_loc e
    ~on_error:(fun () -> Failed)
    (fun xs -> Parsed ((),xs) )

let wrap e (cond: info -> bool) =
  match qmlargs e with
  | Failed ->
      Format.printf "%s %d\n%!" __FILE__ __LINE__;
      false
  | Parsed ((),xs) ->
(*      Format.printf "Ars length = %d. %s %d\n%!" (List.length xs) __FILE__ __LINE__;*)
      match all xs with
      | Failed ->
        Format.printf "%s %d\n%!" __FILE__ __LINE__;
        false
      | Parsed (rez, []) ->
(*          Format.printf "checking result %s:\t%s %d\n%!" (show_info rez) __FILE__ __LINE__;*)
          cond rez
      | Parsed (_,_) ->
          Format.printf "%s %d\n%!" __FILE__ __LINE__;
          false

let loc = dummy_loc
let%test _ =
  wrap [%expr qml singleton ~name:"asdf" ]
    (fun rez -> rez.is_singleton && (rez.name = (Some "asdf")))




let%test _ =
  wrap [%expr qml singleton]
    (fun rez -> rez.is_singleton && (rez.name = None))

