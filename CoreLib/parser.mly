/* File parser.mly */ 
%token EOF
%token END
%token ARROW
%token <int>IVALUE
%token <float>FVALUE
%token INT
%token FLOAT
%token BELONGS
%token PLUS MINUS TIMES
%token LPAREN RPAREN
%token LBRACE RBRACE
%token LBRACKET RBRACKET
%token TRUE
%token FALSE
%token AFFECT
%token SEQ
%token INC
%token IF
%token THEN
%token ELSE
%token WHILE INVAR DO FOR
%token RETURN
%token NEG
%token CONJ
%token DISJ
%token IMPL
%token EQ
%token EQCOMP
%token LT
%token <string> ID;
%token SKIP
%token FORALL
%token EXISTS
%token COMMA
%left PLUS MINUS
%left TIMES

%start instruction
 
%type <MicroC.instruction> instruction;
%type <MicroC.expression> expression;


%left LT GT EQ NE LE GE
%left PLUS MINUS
%left MULTIPLY DIVIDE

%%
instruction :
	LBRACKET instruction RBRACKET {$2}
  | SKIP { Skip (Value (Tint 0)) }
  | ttype ID EQ expression { (Declare ($1,$2,$4))}
  | instruction SEQ instruction { (Seq ($1,$3))}
  | ID AFFECT expression { (Assign ($1, $3))}
  | IF  LPAREN condition RPAREN LBRACKET instruction RBRACKET ELSE LBRACKET instruction RBRACKET {If ($3,$6,$10)}
  | WHILE LPAREN condition RPAREN LBRACKET instruction RBRACKET {While ($3,$6)}
  | ttype ID LPAREN arg_list RPAREN LBRACKET instruction RBRACKET {Declare (FunctionT ((List.map snd $4), $1),$2,(Value (Tfun ($4,$1,$7))))}
  | ID LPAREN expression_list RPAREN {Call ($1,$3)}
  | RETURN expression {(Return $2)}
  | ID INC {(Assign ($1,(BinOp (Plus,(Variable $1),(Value (Tint 1))))))}
  ;

  condition : 
  LPAREN condition RPAREN {$2}
  | TRUE {TRUE}
  | FALSE {FALSE}
  | NEG condition         {(Neg $2)}
  | condition CONJ condition   {(CondBin (And,$1,$3))}
  | condition DISJ condition   {(CondBin (Or,$1,$3))}
  | expression EQCOMP expression {(Compare (Eq, $1,$3))}
  | expression LT expression {(Compare (Lt, $1,$3))}
  ;
  
  expression :
  LPAREN expression RPAREN {$2}
  | ID {(Variable $1)}
  | IVALUE {(Value (Tint $1))}
  | FVALUE {(Value (Tfloat $1))}
  | expression PLUS expression {(BinOp (Plus,$1,$3))}
  | expression MINUS expression {(BinOp (Minus,$1,$3))}
  | expression TIMES expression {(BinOp (Mult,$1,$3))}
  | ID LPAREN expression_list RPAREN {Callexp (Call ($1,$3))}
  ;

arg_list:
{[]}
| ID BELONGS ttype { [($1,$3)]}
| ID BELONGS ttype COMMA arg_list { (($1,$3)::$5) }

ttype : 
INT { Int }
| FLOAT { Float }
| LPAREN ttype_list RPAREN { (MicroC.function_type_from_list $2 []) }
;

ttype_list :
ttype {[$1]}
| ttype ARROW ttype_list { $1::$3 }
;

expression_list:
{([])}
| expression {([$1])}
| expression COMMA expression_list {$1::$3}
;

	   
	   
			
		   
		    
		    
		    
