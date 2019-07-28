type token =
  | EOF
  | END
  | INT of (int)
  | PLUS
  | MINUS
  | TIMES
  | LPAREN
  | RPAREN
  | LBRACE
  | RBRACE
  | AFFECT
  | SEQ
  | IF
  | THEN
  | ELSE
  | WHILE
  | INVAR
  | DO
  | OD
  | NEG
  | CONJ
  | DISJ
  | IMPL
  | EQ
  | LT
  | ID of (string)
  | SKIP
  | FORALL
  | EXISTS
  | COMMA

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> First_order.prop*Gcl.prog_inst*First_order.prop
