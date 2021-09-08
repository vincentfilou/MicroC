/* File parser.mly */ 
%token EOF
%token END
%token <int>INT
%token PLUS MINUS TIMES
%token LPAREN RPAREN
%token LBRACE RBRACE
%token AFFECT
%token SEQ
%token IF
%token THEN
%token ELSE
%token WHILE INVAR DO FOR
%token NEG
%token CONJ
%token DISJ
%token IMPL
%token EQ
%token LT
%token <string> ID;
%token SKIP
%token FORALL
%token EXISTS
%token COMMA
%left PLUS MINUS
%left TIMES

%start main
 
%type <instruction>main;
      %%
	main:
	  instruction EOF{$1};

      instruction :
	LPAREN instruction RPAREN {$2}
     | SKIP { Skip }
     |ID AFFECT prog_value { (Affect (Prog_Variable $1, $3))}
     |inst SEQ inst {Seq ($1,$3)}
     |IF pprop THEN inst ELSE inst {If ($2,$4,$6)}
     |WHILE pprop INVAR prop DO inst OD {While ($2,$4,$6)}
     ;
       pprop:
	 pprop CONJ pprop {Gcl.Conj ($1,$3)}
     |  NEG pprop         {Gcl.Neg  $2}
     |  pprop DISJ pprop   {Gcl.Disj ($1,$3)}
     |  prog_value EQ prog_value    {Gcl.Eq   ($1,$3)}
     |  prog_value LT prog_value    {Gcl.Lt   ($1,$3)};

	prog_value:
	  ID {Prog_Variable $1}
     | INT  {Prog_Value $1}
     | prog_value PLUS prog_value {Prog_Plus ($1,$3)}
     | prog_value MINUS prog_value {Prog_Minus ($1,$3)}
     | prog_value TIMES prog_value  {Prog_Mult ($1,$3)};

       value:
	 ID {First_order.Variable $1}
     | INT  {First_order.Value $1}
     | value PLUS value {First_order.Plus ($1,$3)}
     | value MINUS value {First_order.Minus ($1,$3)}
     | value TIMES value  {First_order.Mult ($1,$3)};
	 
       
       prop:
      LPAREN prop RPAREN { $2 }
     | NEG prop { First_order.Neg $2}
     | prop CONJ prop {First_order.Conj ($1,$3)}
     | prop DISJ prop {First_order.Disj ($1,$3)}
     | prop IMPL prop {First_order.Impl ($1,$3)}
     | value EQ value {First_order.Eq ($1,$3)}
     | value LT value {First_order.Lt ($1, $3)}
     | FORALL ID COMMA prop {First_order.Forall ($2,$4)}
     | EXISTS ID COMMA prop {First_order.Exist ($2,$4)}
     ;
    
	   
	   
			
		   
		    
		    
		    
