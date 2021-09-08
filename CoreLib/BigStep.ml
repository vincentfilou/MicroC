open MicroC;;
open Env;;

exception Unexpected_instruction of instruction;;

type ret_val_inst = | Retval of int | Break of int;;

let value_of_retval_inst x = (match x with | Retval x -> x | Break x -> x);;

(* idea :  build derivation tree with rules names ? using latex and bussproofs ? 
build_tree : string->int->(env*instruction)->(env*instruction)->(env*instruction)
call build_tree on result, push, then pop to build latex doc (the first rule is the one on top of the stack)

example :  | (e, Skip _) -> (build_tree "Skip" 0 x (e,Retval 0))
with 0 for an axiom, 1 for an unary inference rule, etc...
*)

let rec eval (x:environment*instruction):(environment*ret_val_inst) =
  (match x with 
  | (e, Skip _) -> (e,Retval 0)

  | (e, Declare (id,exp)) -> let (e',x) = eval_expression (e,exp) in ((declare e' id x), (Retval x))

  | (e, Seq (i0,i1)) -> let (e',x) = eval (e,i0) in 
    (match x with | Break v -> (e',Retval v) 
                  | _       -> eval (e',i1))

  | (e,Assign (id,exp)) -> let (e',v) = eval_expression (e,exp) in ((set e' id v),Retval v)

  | (e,If (c0,i0,i1))   -> let (e',b) = eval_condition (e,c0) in if b then (eval (e',i0)) else (eval (e',i1))

  | (e,While (c0,i0))    -> let (e',b) = eval_condition (e,c0) in if (not b) then (e',Retval 0) else (
    let (e'',_) = (eval (e',i0)) in (eval (e'', (snd x))))
  

  | (e,Defun (id,largs,i0)) -> ((function_declare e id largs i0),Retval 0)

  (*type : check that id is a function and that the args are all defined
  modify function_get to  get + check type is function ?
  Call expression -> lexp -> instruction
        + expression reduces to a function
  *)
  | (e, Call (id,lexp))      -> 
    let (largs,def) = (function_get e id) in
    let (e',values) = (eval_args e lexp) in
    let e'' =  (declare_all (open_scope e') largs values) in 
    let (e''',v) = eval (e'',def) in ((close_scope e'''),v)

  | (e, Return exp) -> let (e',v) = eval_expression (e,exp) in (e',Break  v)
  | (_, i )         -> raise (Unexpected_instruction i)
  )

  (* types implementation : eval_expression x -> TypedValue *)
  (*  get : env -> id -> TypedValue 
      binop : op -> TypedValue -> TypedValue -> TypedValue , si typeof v0 = typeof v1 & typeof v0 <> function ?
      compare : op -> TypedValue -> TypedValue -> bool, si typeof v0 = typeof v1
  *)

  and eval_expression (x:environment*expression) = 
  (match x with
  | (e, Variable id) -> (e,(get e id))
  | (e, Value x)  -> (e,x)
  | (e, BinOp (op,e0,e1)) -> let (e',x) = (eval_expression (e,e0)) in 
                             let (e'',x') = eval_expression (e',e1) in
                             (e'', match op with | Plus -> x+x' | Minus -> x - x' | Mult -> x *x')
  | (e, Callexp i0) -> let (e',v) = (eval (e,i0)) in (e',(value_of_retval_inst v))
  )
  
  and eval_condition (x:environment*condition) = 
  (match x with

  | (e,TRUE)  -> (e,true)

  | (e,FALSE) -> (e,false)

  | (e , Neg c0) -> let x = eval_condition (e,c0) in ((fst x),not (snd x)) 

  | (e, CondBin (op,e0,e1)) -> 
      (match op with | And -> let (e',b) = eval_condition (e,e0) in (if b then eval_condition (e',e1) else (e',false))
                     | Or  -> let (e',b) = eval_condition (e,e0) in (if b then (e',true) else (eval_condition (e',e1))))

  | (e,Compare (op,e0,e1)) -> let (e',v)   = eval_expression (e,e0) in 
                              let (e'',v') = eval_expression (e',e1) in 
                              (e'', match op with | Eq -> v=v' | Lt -> v < v') 
  
  )
  and  eval_args (e:environment)(lexp: expression list): (environment*(int list)) = 
    (match lexp with 
    | [] -> (e,[])
    | exp::lexp0 -> let (e',x) = eval_expression (e,exp) in 
            let x'' = (eval_args e' lexp0) in ((fst x''),x::(snd x''))
    )
;;