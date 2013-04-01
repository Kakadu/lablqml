open StdLabels
open Parser
open Printf

external (|>): 'a -> ('a -> 'b) -> 'b  = "%revapply"

let make s =
  let buffer = Lexing.from_string s in
  Location.init buffer "";
  Location.input_name := "";

  let rich_info = ref [] in
  let add_info x = rich_info := x :: !rich_info in
  let () = try
    while true do
      let token = Lexer.token buffer
      and start = Lexing.lexeme_start buffer
      and stop = Lexing.lexeme_end buffer in
      let tag = match token with
        | AND      | AS       | BAR      | CLASS    | CONSTRAINT | EXCEPTION    | EXTERNAL
        | FUN      | FUNCTION | FUNCTOR  | IN       | INHERIT    | INITIALIZER  | LET
        | METHOD   | MODULE   | MUTABLE  | NEW      | OF         | PRIVATE      | REC
        | TYPE     | VAL      | VIRTUAL  -> "define"
        | EOF -> raise End_of_file
        | _ -> ""
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

  List.iter (List.rev !rich_info) ~f:(fun (start,stop,token,tag) ->
    if !lastpos < start
    then Buffer.(
      let s = StringLabels.sub s ~pos:(!lastpos) ~len:(start - !lastpos) in
      let s = Str.global_replace (Str.regexp "\n") "<br/>" s in
      let s = Str.global_replace (Str.regexp " ") "&nbsp;" s in
      add_string ans s;
    );
    let () = match tag with
      | "define" ->
          Buffer.(
            add_string ans "<font color=\"blue\">";
            add_string ans token;
            add_string ans "</font>";
          );
      |  _ -> Buffer.add_string ans token
    in
    lastpos := stop;
  );
  Buffer.contents ans
