%{
  open TypAst
%}
%token INT FLOAT STRING BOOL STAR LBRA RBRA UNIT MOUSEEVENT GS_MOUSEEVENT KEYEVENT
%token LIST
%token EOL
%left  STAR
%start main
%type <TypAst.t> main
%type <TypAst.t> expr
%%
main:
  expr EOL { $1 }
;
expr:
  INT            { `Int }
| UNIT           { `Unit }
| FLOAT          { `Float }
| MOUSEEVENT     { `QMouseEvent }
| GS_MOUSEEVENT  { `QGMouseEvent }
| KEYEVENT       { `QKeyEvent   }
| BOOL           { `Bool }
| STRING         { `String }
| expr LIST      { `List $1 }
| LBRA expr RBRA { $2 }
| expr STAR expr { match $1 with
                  | `Tuple lst -> `Tuple (lst @ [$3])
                  | x          -> `Tuple [x;$3] }
%%
