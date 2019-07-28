{
open Parser
}

rule token = parse
[' ' '\t' '\n'] { token lexbuf }
| ";;" {END}
| "[" {LBRACE}
| "]" {RBRACE}
|"+" {PLUS}
|"-" {MINUS}
|"*" {TIMES}
|"(" {LPAREN}
|")" {RPAREN}
| "skip" {SKIP}
|":=" {AFFECT}
|";" {SEQ}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "while" {WHILE}
| "invariant" {INVAR}
| "do" {DO}
| "od" {OD}
| "not" {NEG}
| "/\\" {CONJ}
|"\\/" {DISJ}
| "=" {EQ}
| "<" {LT}
| "->" {IMPL}
| "forall" {FORALL}
| "exists" {EXISTS}
| "," {COMMA}
|['0'-'9']+ as lxm {INT (int_of_string lxm)}
|['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as id {ID id}
| eof { EOF }
