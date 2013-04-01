open StdLabels
open Parser
open Printf

external (|>): 'a -> ('a -> 'b) -> 'b  = "%revapply"
type tag =
  | Control | Define | Structure | Char | Infix | Label | UIndent

(* Next colors are from labltk. Some of them can be invalid. TODO: check this *)
let color_of_tag = function
  | Control -> "blue"
  | Define  -> "forestgreen"
  | Structure -> "purple"
  | Char   -> "gray"
  | Infix  -> "indianred4"
  | Label  -> "saddlebrown"
  | UIndent -> "midnightblue"

let make s =
  let buffer = Lexing.from_string s in
  Location.init buffer "";
  Location.input_name := "";

  let rich_info = ref [] in
  let add_info x = rich_info := x :: !rich_info in
  let () = try
    let last = ref (EOF, 0, 0) in
    while true do
      let token = Lexer.token buffer
      and start = Lexing.lexeme_start buffer
      and stop = Lexing.lexeme_end buffer in
      let tag = match token with
        | AMPERAMPER | AMPERSAND | BARBAR | DO | DONE | DOWNTO | ELSE  | FOR
        | IF       | LAZY        | MATCH  | OR | THEN | TO     | TRY   | WHEN
        | WHILE    | WITH          -> Some Control
        | AND      | AS       | BAR      | CLASS    | CONSTRAINT | EXCEPTION    | EXTERNAL
        | FUN      | FUNCTION | FUNCTOR  | IN       | INHERIT    | INITIALIZER  | LET
        | METHOD   | MODULE   | MUTABLE  | NEW      | OF         | PRIVATE      | REC
        | TYPE     | VAL      | VIRTUAL  -> Some Define
        | BEGIN    | END      | INCLUDE      | OBJECT      | OPEN      | SIG
        | STRUCT     -> Some Structure
        | CHAR _   | STRING _ -> Some Char
        | INFIXOP1 _        | INFIXOP2 _        | INFIXOP3 _        | INFIXOP4 _        | PREFIXOP _
        | BACKQUOTE | SHARP          ->  Some Infix
        | LABEL _        | OPTLABEL _        | QUESTION        | TILDE          ->  Some Label
        | UIDENT _ -> Some UIndent
        | LIDENT _ ->
            begin match !last with
              | (QUESTION | TILDE), _, _ ->  Some Label
              | _ -> None
            end
        | COLON -> begin
            match !last with
              | (LIDENT x, lstart, lstop) ->
                  if lstop = start then add_info (lstart,lstop,x, Some Label);
                  None
              | _ -> None
          end
        | EOF -> raise End_of_file
        | _ -> None
      in
      add_info (start,stop,StringLabels.sub ~pos:start ~len:(stop-start) s,tag);
    done
  with
    | End_of_file -> ()
    | Lexer.Error (err, loc) -> ()
  in
  let lastpos = ref 0 in
  let len = String.length s in
  let ans = Buffer.create len in

  let rich_info = List.rev !rich_info in
  let () = if rich_info <> [] then
      List.fold_left ~init:(List.hd rich_info) (List.tl rich_info)
        ~f:(fun (_,e,_,_) ((_,s,_,_)as y) -> assert (e<s); y) |> ignore
  in
  List.iter rich_info ~f:(fun (start,stop,token,tag) ->
    if !lastpos < start
    then Buffer.(
      let s = StringLabels.sub s ~pos:(!lastpos) ~len:(start - !lastpos)
                    |> Str.global_replace (Str.regexp "\n") "<br/>"
                    |> Str.global_replace (Str.regexp " ") "&nbsp;" in
      add_string ans s;
    );
    let () = match tag with
      | Some x -> Buffer.add_string ans (sprintf "<font color='%s'>%s</font>" (color_of_tag x) token)
      |  _ -> Buffer.add_string ans token
    in
    lastpos := stop;
  );
  Buffer.contents ans
