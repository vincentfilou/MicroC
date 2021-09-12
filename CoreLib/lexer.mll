{
open Parser
}

rule token = parse
[' ' '\t' '\n'] { token lexbuf }
| "->"  {ARROW}
| "int" {INT}
| "float" {FLOAT}
| ":" {BELONGS} 
| ";;" {END}
| "{" {LBRACKET}
| "}" {RBRACKET}
| "[" {LBRACE}
| "]" {RBRACE}
|"+" {PLUS}
|"-" {MINUS}
|"*" {TIMES}
|"(" {LPAREN}
|")" {RPAREN}
| "true" {TRUE}
| "false" {FALSE}
| "skip" {SKIP}
|":=" {AFFECT}
|";" {SEQ}
| "++" {INC}
| "if" {IF}
| "then" {THEN}
| "else" {ELSE}
| "while" {WHILE}
| "invariant" {INVAR}
| "do" {DO}
| "for" {FOR}
| "return" {RETURN}
| "not" {NEG}
| "&&" {CONJ}
|"||" {DISJ}
| "=" {EQ}
| "==" {EQCOMP}
| "<" {LT}
| "=>" {IMPL}
| "forall" {FORALL}
| "exists" {EXISTS}
| "," {COMMA}
|['0'-'9']+ as lxm {IVALUE (int_of_string lxm)}
|['0'-'9']+'.'['0'-'9']+ as lxm {FVALUE (float_of_string lxm)}
|['A'-'Z' 'a'-'z']['A'-'Z' 'a'-'z' '0'-'9' '_']* as id {ID id}
| eof { EOF }
