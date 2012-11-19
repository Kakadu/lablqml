{
open TypParser
}

rule token = parse
  ['\t' ' ']                { token lexbuf }
| ['\n'   ]                 { EOL }
| "int"                     { INT }
| "bool"                    { BOOL }
| "float"                   { FLOAT }
| "string"                  { STRING }
| "unit"                    { UNIT }
| "*"                       { STAR }
| "list"                    { LIST }
| '('                       { LBRA }
| ')'                       { RBRA }

{
let parse_string s = 
  let buf = Lexing.from_string (s^"\n") in
  main token buf 
}
