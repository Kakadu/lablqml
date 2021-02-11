open Ppxlib

let dummy_loc = Location.in_file "dummy.ml"

type stream_item = arg_label * expression

type 'a parse_result =
  | Failed
  | Parsed of 'a * stream_item list

type 'a parser = stream_item list -> 'a parse_result

let return x xs = Parsed (x, xs)

let pnil : unit parser =
 fun xs ->
  match xs with
  | [] -> return () []
  | _ -> Failed
;;

let myparse p onOK xs = Ast_pattern.parse p dummy_loc xs ~on_error:(fun () -> Failed) onOK

let args_end : unit parser =
 fun xs ->
  (*  Format.printf "calling args_end %s %d\n%!" __FILE__ __LINE__;*)
  Ast_pattern.parse
    Ast_pattern.nil
    dummy_loc
    xs
    ~on_error:(fun () -> Failed)
    (Parsed ((), []))
;;

let singleton : unit parser =
  let open Ast_pattern in
  let p = (no_label @@ pexp_ident (lident (string "singleton"))) ^:: __ in
  myparse p (fun tl -> Parsed ((), tl))
;;

let named_string name : string parser =
  let open Ast_pattern in
  let p =
    pair (labelled (string name)) (pexp_constant @@ pconst_string __ __ none) ^:: __
  in
  myparse p (fun name _ tl -> Parsed (name, tl))
;;

let name = named_string "name"

let the_ident : string -> unit parser =
 fun s stream ->
  let open Ast_pattern in
  let p = (no_label @@ pexp_ident (lident (string s))) ^:: __ in
  myparse p (fun xs -> Parsed ((), xs)) stream
;;

let pident : string parser =
  let open Ast_pattern in
  let p = (no_label @@ pexp_ident (lident __)) ^:: __ in
  myparse p (fun s xs -> Parsed (s, xs))
;;

let ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser =
 fun p f xs ->
  match p xs with
  | Failed -> Failed
  | Parsed (x, tl) -> f x tl
;;

let many : 'a parser -> 'a list parser =
 fun p ->
  let rec helper acc input =
    match p input with
    | Failed -> return (List.rev acc) input
    | Parsed (x, tl) -> helper (x :: acc) tl
  in
  helper []
;;

let ( >> ) : 'a parser -> 'b parser -> 'b parser = fun p q -> p >>= fun _ -> q

let ( <* ) : 'a parser -> 'b parser -> 'a parser =
 fun p q -> p >>= fun x -> q >>= fun _ -> return x
;;

let ( <|> ) p q xs =
  match p xs with
  | Failed -> q xs
  | r -> r
;;

let choice ps input =
  let rec helper = function
    | [] -> Failed
    | h :: ps ->
      (match h input with
      | Failed ->
        (*          Format.printf "choiced parser failed: %s %d\n%!" __FILE__ __LINE__;*)
        helper ps
      | Parsed (r, tl) -> Parsed (r, tl))
  in
  helper ps
;;

let wrap head parsef e ~onfail cond =
  match head e with
  | Failed ->
    Format.printf "head failed : %s %d\n%!" __FILE__ __LINE__;
    onfail
  | Parsed (head_result, xs) ->
    (*      Format.printf "Ars length = %d. %s %d\n%!" (List.length xs) __FILE__ __LINE__;*)
    (match parsef xs with
    | Failed ->
      (*        Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
      onfail
    | Parsed (rez, []) ->
      (*          Format.printf "calling cond \n%!";*)
      cond head_result rez
    | Parsed (_, _) ->
      (*          Format.printf "%s %d\n%!" __FILE__ __LINE__;*)
      onfail)
;;

let loc = dummy_loc

(* ******************************************************** *)
type prop_name = string [@@deriving show]
type prop_typ = string [@@deriving show]

type prop_info =
  { mutable p_read : string option
  ; mutable p_write : string option
  ; mutable p_notify : string option
  }
[@@deriving show]

let make_prop_info () = { p_read = None; p_write = None; p_notify = None }

let prop_head : expression -> (prop_name * prop_typ) parse_result =
 fun e ->
  let p =
    let open Ppxlib.Ast_pattern in
    pexp_apply (pexp_constraint (pexp_ident (lident __)) (ptyp_constr (lident __) nil)) __
  in
  Ast_pattern.parse
    p
    dummy_loc
    e
    ~on_error:(fun () -> Failed)
    (fun name typ xs -> Parsed ((name, typ), xs))
;;

let try_ p xs =
  match p xs with
  | Parsed (x, tl) -> return (Some x) tl
  | Failed -> return None xs
;;

let parse_body : prop_info parser =
  let ptag_wrap name =
    let open Ast_pattern in
    let p =
      (no_label @@ pexp_construct (lident (string name)) none)
      ^:: (no_label @@ pexp_ident (lident __))
      ^:: __
    in
    myparse p (fun name tl -> Parsed (name, tl))
  in
  let pwrite = ptag_wrap "WRITE" in
  let pread = ptag_wrap "READ" in
  let pnotify = ptag_wrap "NOTIFY" in
  fun xs ->
    let rez = make_prop_info () in
    xs
    |> (many
        @@ choice
             [ (pwrite
               >>= fun s ->
               rez.p_write <- Some s;
               return ())
             ; (pread
               >>= fun s ->
               rez.p_read <- Some s;
               return ())
             ; (pnotify
               >>= fun s ->
               rez.p_notify <- Some s;
               return ())
             ]
       >> args_end
       >> return rez)
;;

let wrap_prop e cond = wrap prop_head parse_body e cond

(* ************************************************************************** *)
type info =
  { mutable is_singleton : bool
  ; mutable name : string option
  ; mutable namespace : string option
  ; mutable props : (prop_name * prop_typ * prop_info) list
  }
[@@deriving show]

let empty_info = { is_singleton = false; name = None; namespace = None; props = [] }

type prop_item = prop_name * prop_typ * prop_info [@@deriving show]

type data_item =
  | ISingleton of bool
  | IProp of prop_item
  | IName of string
  | INamespace of string
[@@deriving show]

(*let data_items = data_item list [@@deriving show]*)
let show_data_items xs = List.map show_data_item xs |> String.concat ","

let parse_props : data_item parser =
 fun stream ->
  match stream with
  | [] -> Failed
  | (_lab, h) :: other_exprs ->
    let p = function
      | [] -> Failed
      | (_, x) :: _ ->
        (match prop_head x with
        | Failed -> Failed
        | Parsed ((_, _), []) ->
          (* not enough arguments for property description *)
          Failed
        | Parsed ((name, typ), other_prop_info) ->
          (parse_body >>= fun pinfo -> return (name, typ, pinfo)) other_prop_info)
    in
    (match (p <* pnil) [ Nolabel, h ] with
    | Failed -> Failed
    | Parsed (p, []) -> return (IProp p) other_exprs
    | Parsed (_, _) -> assert false)
;;

let all : info parser =
 fun xs ->
  xs
  |> (many
      @@ choice
           [ (named_string "name" >>= fun s -> return (IName s))
           ; (named_string "namespace" >>= fun s -> return (INamespace s))
           ; (singleton >>= fun () -> return (ISingleton true))
           ; parse_props
           ]
     >>= fun extra ->
     args_end
     >> (*    Format.printf "%s\n%!" (show_data_items extra);*)
     return
     @@ List.fold_left
          (fun rez x ->
            match x with
            | IName s -> { rez with name = Some s }
            | INamespace s -> { rez with namespace = Some s }
            | ISingleton b -> { rez with is_singleton = b }
            | IProp p -> { rez with props = rez.props @ [ p ] })
          empty_info
          extra)
;;

let qmlargs : expression -> unit parse_result =
  let p =
    let open Ppxlib.Ast_pattern in
    pexp_apply (pexp_ident (lident (string "qml"))) __
  in
  myparse p (fun xs -> Parsed ((), xs))
;;

let singleton : expression -> unit parse_result =
  let p =
    let open Ppxlib.Ast_pattern in
    pexp_apply (pexp_ident (lident (string "singleton"))) __
  in
  fun xs ->
    let myOK r =
      match xs.pexp_desc with
      | Pexp_apply (e, args) -> Parsed ((), (Nolabel, e) :: args)
      | _ -> failwith "should not happen"
    in
    myparse p myOK xs
;;

let wrap_qml e cond = wrap qmlargs all e (fun () -> cond)
let wrap_singleton e cond = wrap singleton all e (fun () -> cond)

(*let%test _ =
  wrap_prop ~onfail:false
    [%expr (author: string) READ getAuthor ]
    (fun (name,typ) rez ->
      (name="author") && (typ="string") && (rez.p_read = Some "getAuthor")
      && (rez.p_notify = None) && (rez.p_write = None)
    )*)

(*let%test _ =
  wrap_prop ~onfail:false
    [%expr (author: string) READ getAuthor WRITE setAuthor NOTIFY authorChanged ]
    (fun (name,typ) rez ->
(*      Format.printf "name=%s, typ=%s, rez = %s\n%!" name typ (show_prop_info rez);*)
      (name="author")
      && (typ="string")
      && (rez.p_read   = Some "getAuthor")
      && (rez.p_write  = Some "setAuthor")
      && (rez.p_notify = Some "authorChanged")
    )*)

let%test _ =
  wrap_singleton
    ~onfail:false
    [%expr singleton ~name:"mySingleton"]
    (*    (fun () xs -> xs)*)
    (fun rez ->
      (*      Format.printf "rez = %s\n%!" (show_info  rez);*)
      rez.is_singleton && rez.name = Some "mySingleton")
;;

let%test _ =
  wrap_singleton
    ~onfail:false
    [%expr
      singleton
        ~name:"qwe"
        ((author : string) READ getAuthor WRITE setAuthor NOTIFY authorChanged)
        ((event : int) READ event NOTIFY eventChanged)]
    (fun rez ->
      rez.name = Some "qwe"
      &&
      match rez.props with
      | [ ( "author"
          , "string"
          , { p_read = Some "getAuthor"
            ; p_write = Some "setAuthor"
            ; p_notify = Some "authorChanged"
            } )
        ; ( "event"
          , "int"
          , { p_read = Some "event"; p_write = None; p_notify = Some "eventChanged" } )
        ] -> true
      | _ -> false)
;;

let%test _ =
  wrap_singleton ~onfail:false [%expr singleton ~name:"MyApi"] (fun rez ->
      print_endline @@ show_info rez;
      rez.is_singleton && rez.name = Some "MyApi" && [] = rez.props)
;;

let parse_singleton e = wrap singleton all e ~onfail:None (fun () -> Base.Option.some)
