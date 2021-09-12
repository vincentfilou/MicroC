open MicroC;;
open Env;;

(* step 2 : expression and condition will modify the environment *)
(* TODO semantic + implementation + tests *)
(* OK : function call inside funcall for handling return ? *)


exception Undefined_Parameter of string
exception Incomplete_Evaluation of string


(* all parameters must be evaluated/reduced to a value. They are evaluated from left to right*)

(* defun x y z = i0 -> declare x (defun y z = i0) ... open a new scope each time ? are scopes necessary ? 
yes, to pop. can't open a new scope each time -> only one return 
return : close scope only when evaluation finishes
*)

(* remove the skip for functions, and replace in eval_expression (Function (Return exp)) ?
we have to close the scope *)

(* TODO : delete (declare_all in env?)*)

let rec init_scope (e:eval_environment)(la:string list)(lv:expression list) =
  match (la,lv) with
  | ([],_)                   -> e
  | (id::_,[])             -> raise (Undefined_Parameter id)
  | (id::la0,(Value v)::lv0) -> (declare (init_scope e la0 lv0) id v)
  | (id::_,_::_)         -> raise (Incomplete_Evaluation id)
;;

let rec args_fully_evaluated (lv:expression list) = 
  match lv with
  | []            -> true
  | (Value _) :: l0 -> args_fully_evaluated l0
  | _::_          -> false
;;


(* TODO : ? list all rules in main match, to correspond to the "docs" ? or adapt doc with one section per instruction *)
(* Note skip always return a value. Syntactic sugar Skip -> Skip (Value 0), same for return*)

let rec eval (x:(eval_environment*instruction)) = 
  match x with 

  | (e, Skip exp)         -> 
      (match exp with 
        | Value v -> (e,Skip (Value v)) 
        | _ -> let x0 = (eval_expression (e,exp)) in ((fst x0), Skip (snd x0)))

  | (e, Declare (tt0,id,exp)) -> 
      (match exp with 
        | Value v -> ((declare e id v), Skip (Value (Tint 0)))
        | _       -> let x0 = (eval_expression (e,exp)) in ((fst x0), Declare (tt0,id,(snd x0))))

  | (e, Seq (i0,i1))      -> 
    (match i0 with 
    | Skip _     -> (e,i1) 
    | Return exp -> (e,Return exp)
    | _          -> match (eval (e,i0)) with (e0,i2) -> (e0, Seq (i2,i1)))
 
  | (e, Assign (id,exp))  -> 
      ( match exp with
        | Value v -> ((set e id v),Skip (Value (Tint 0)))
        | _       -> let x0 = (eval_expression (e,exp)) in ((fst x0), (Assign (id,(snd x0)))) 
      )
    
  | (e, If (c0,i0,i1))    -> 
      (match c0 with 
      | TRUE -> (e,i0)
      | FALSE -> (e,i1)
      | _     -> let x0 = eval_condition (e,c0) in ((fst x0),(If (snd x0,i0,i1)))
      )

  | (e, While (c0,i0))    -> (e, (If (c0,(Seq (i0,(While (c0,i0)))),(Skip (Value (Tint 0))))))

  | (e, Call (id, largs)) -> 
    if args_fully_evaluated largs then 
      (let f = (get e id) in 
      match f with 
        | Tfun  (l,_,def) -> ((init_scope (open_scope e) (List.map (fun x -> fst x) l) largs), (Function def))
        | _ -> raise Type_Exception
        )
    else let x0 = (eval_list_args (e,largs)) in (fst x0, (Call (id,(snd x0))))

    | (e, Function i0) ->
      (match i0 with 
      | Skip   (Value v) ->  ((close_scope e), (Skip (Value v)))
      | Return (Value v) -> ((close_scope e), (Skip (Value v)))
      | _       -> let x0 = (eval (e,i0)) in ((fst x0), Function (snd x0))
      )

    | (e, Return exp) -> 
      match exp with 
      | (Value _) -> (e, Return exp)
      | _       -> let x0 = (eval_expression (e,exp)) in ((fst x0), Return (snd x0))

  and eval_expression (x:(eval_environment*expression)) = 
  (match x with 

  | (e, Variable id)       -> (e,(Value (get e id)))

  | (_, Value x0)          -> ((fst x),(Value x0))

  | (e, (BinOp (op,exp0,exp1))) ->
    (match (exp0,exp1) with 
    | ((Value v0), (Value v1)) -> (e,Value (eval_binop op v0 v1))
    | ((Value v0), _)          -> let x0 = (eval_expression (e,exp1)) in ((fst x0), (BinOp (op, (Value v0), (snd x0))))
    | (_,_)                    -> let x0 = (eval_expression (e,exp0)) in ((fst x0), (BinOp (op, (snd x0), exp1)))
    )
  
  | (e, Callexp i0) -> 
    (match i0 with 
    | Skip (Value v) -> (e,(Value v))
    | _              -> let x0 = (eval (e,i0)) in ((fst x0), (Callexp (snd x0)))
    )
)
  and eval_condition (x:eval_environment*condition) = 
  match x with 

  | (e, Neg c0)         -> 
    (match c0 with 
    | TRUE  -> (e,FALSE)
    | FALSE -> (e,TRUE)
    | _     -> let x0 = (eval_condition (e,c0)) in ((fst x0),(Neg (snd x0)))
    )

  | (e, (CondBin (op,c0,c1)))     -> 
    (match (c0,c1) with | (TRUE,TRUE)    -> (match op with | And -> (e,TRUE)  | Or  -> (e,TRUE))
                        | (TRUE, FALSE)  -> (match op with | And -> (e,FALSE) | Or -> (e,TRUE))
                        | (FALSE, TRUE)  -> (match op with | And -> (e,FALSE) | Or -> (e,TRUE))
                        | (FALSE, FALSE) -> (match op with | And -> (e,FALSE) | Or -> (e,FALSE))
                        | (TRUE,_)       -> let x0 = eval_condition (e,c1) in ((fst x0), (CondBin(op,c0,(snd x0))))
                        | (FALSE, _)     -> let x0 = eval_condition (e,c1) in ((fst x0), (CondBin(op,c0,(snd x0))))
                        | (_,_)          -> let x0 = eval_condition (e,c0) in ((fst x0), (CondBin(op,(snd x0),c1)))
    )
    
  | (e, Compare (op,exp0,exp1)) ->
    (match (exp0,exp1) with 
      | ((Value v0),(Value v1)) -> (match op with | Lt -> (if v0 < v1 then (e,TRUE) else (e,FALSE))
                                                  | Eq -> (if v0 = v1 then (e,TRUE) else (e,FALSE)))
      | ((Value _),_)        -> let x0 = (eval_expression (e,exp1)) in ((fst x0),(Compare (op,exp0,(snd x0))))
      | (_ , _)               -> let x0 = (eval_expression (e,exp0)) in ((fst x0),(Compare (op,(snd x0),exp1)))
    )
  | (e, TRUE) -> (e, TRUE)
  | (e, FALSE) -> (e,FALSE)

  and eval_list_args (x:eval_environment*(expression list)) = 
  match x with 
  | (e,[])          -> (e,[])
  | (e,(Value v)::l0) -> let x0 = (eval_list_args (e,l0)) in ((fst x0), (Value v)::(snd x0))
  | (e, exp0::l0)   -> let x0 = eval_expression (e,exp0) in ((fst x0), ((snd x0)::l0))
;;

let rec eval_loop (hook: (eval_environment*instruction->unit))(x:eval_environment*instruction) =
  hook x;
  (match x with 
  | (_, Skip (Value _)) -> x
  | (_, Return (Value _)) -> x 
  | (_,_) -> (eval_loop hook (eval x))
  )
;;

let simple_eval_loop = eval_loop (fun _->());;

