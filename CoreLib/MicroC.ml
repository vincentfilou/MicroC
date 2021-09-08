
type bin_op = | Plus | Minus | Mult;;
type comp_op = | Lt | Eq;;
type cond_bin_op = | And | Or;;

type expression =
  | Variable of string
  | Value   of int
  | BinOp   of bin_op*expression*expression
  | Callexp of instruction
and  condition = 
  | TRUE
  | FALSE 
  | Neg of condition
  | CondBin of cond_bin_op*condition*condition
  | Compare  of comp_op*expression*expression
and instruction =
  | Skip     of expression
  | Declare  of string*expression
  | Seq      of instruction*instruction
  | Assign   of string*expression
  | If       of condition*instruction*instruction
  | While    of condition*instruction
  | Defun    of string*(string list)*instruction
  | Call     of string*(expression list)
  | Function of instruction
  | Return   of expression
;;

open String;;

let rec instruction_to_string i0 = 
  (match i0 with 
  | Skip exp -> concat " " ["Skip"; expression_to_string exp]
  | Declare (id,exp) -> concat " " ["Declare";id; expression_to_string exp]
  | Seq (i0,i1) -> concat "" ["(";instruction_to_string i0;";\n";instruction_to_string i1;")"]
  | Assign (id,exp) -> concat "" [id;":=";expression_to_string exp]
  | If (c0,i0,i1) -> concat "" ["if (";condition_to_string c0;")\n{";instruction_to_string i0;"\n}else{\n";instruction_to_string i1;"\n}"]
  | While (c0,i0) -> concat "" ["While (";condition_to_string c0;"){\n";instruction_to_string i0;"\n}"]
  | Defun (id,l,i0)  -> (concat "" ["Defun ";id;" (";(concat ";" l);") = {"; instruction_to_string i0;"}"])
  | Call  (id,largs) -> concat "" [id;"("; (concat "," (List.map expression_to_string largs));")"]
  | Function i0      -> concat "" ["Function (";instruction_to_string i0;")"] 
  | Return exp       -> concat "" ["Return (";expression_to_string exp;")"]
  )
  and expression_to_string exp =
  (match exp with
  | Variable id -> id
  | Value v     -> (string_of_int v)
  | BinOp (op,exp0,exp1) ->
      let op_to_string op = match op with | Plus -> "+" | Minus -> "-" | Mult -> "*" in
    (concat "" [expression_to_string exp0;op_to_string op; expression_to_string exp1])
  | Callexp i0 -> instruction_to_string i0
  )
  and condition_to_string cond =
  (match cond with
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | Neg cond0 -> concat "" ["not(";condition_to_string cond0;")"]
  | CondBin (And,cond0,cond1) -> concat "" ["(";condition_to_string cond0;" & ";condition_to_string cond1]
  | CondBin (Or,cond0,cond1) -> concat "" ["(";condition_to_string cond0;" | ";condition_to_string cond1]
  | Compare (Eq,exp0,exp1)   -> concat "" [ expression_to_string exp0;" == ";expression_to_string exp1]
  | Compare (Lt,exp0,exp1)   -> concat "" [ expression_to_string exp0;" < ";expression_to_string exp1]
);;

(* 
module Env = Map.Make(String);; 

type varEnv = (int Env.t);;

let rec eval_expression (env: varEnv)(e : expression) =
  match e with
  | Variable id -> 
      (match (Env.find_opt id env) with 
        Some  x -> x 
      | None -> raise (Undefined_Variable id))
  | Value    v  -> v
  | Plus  (x,y) -> (eval_expression env x) + (eval_expression env y)
  | Minus (x,y) -> (eval_expression env x) - (eval_expression env y)
  | Mult  (x,y) -> (eval_expression env x) * (eval_expression env y)
;;

let rec eval_condition (env: varEnv)(c : condition) =
  match c with
  | Neg e0      -> not (eval_condition env e0)
  | And (e0,e1) -> (eval_condition env e0) && (eval_condition env e1)
  | Or  (e0,e1) -> (eval_condition env e0) || (eval_condition env e1)
  | Eq (e0,e1)  -> (eval_expression env e0) == (eval_expression env e1)
  | Lt (e0,e1)  -> (eval_expression env e0) < (eval_expression env e1)
;;

let rec eval (x : (varEnv*instruction)) =
  match x with 
  | (e, Skip) -> (e,Skip)
  | (e, (Seq (i0,i1))) ->
      (match i0 with 
          Skip -> (e,i1)
        | _    -> (match (eval (e,i0)) with (e0,i2) -> (e0,(Seq (i2,i1)))))
  | (e, (Assign (x,v)))  -> ((Env.add x (eval_expression e v) e), Skip)
  | (e, (If (c,i0,i1)))  -> if (eval_condition e c) then (e,i0) else (e,i1)
  | (e, (While (c,i)))   -> if (eval_condition e c) then (e,Seq (i, (While (c,i)))) else (e,Skip)
;;

let rec eval_loop (x : (varEnv*instruction)) =
  match (eval x) with
  | (e,Skip) -> (e,Skip)
  | (e,i )   -> eval_loop (e,i)
;;

let test_prog = (Seq ((Assign ("x",(Value 10))), (While ((Lt (Value 2,(Variable "x"))),(Assign (("x"), (Minus ((Variable "x"),(Value 1)))))))));; *)


  (**
  | Defun of string*(string list)*instruction
  | Call  of string*(expression list)
*)

(*
type prog_inst =
  Skip
  | Affect of prog_value*prog_value
  | Seq of prog_inst*prog_inst
  | If of prog_prop*prog_inst*prog_inst
  | While of prog_prop*prop*prog_inst
;;

let rec value_of_prog_value v=
  match v with
    Prog_Variable s -> Variable s
  | Prog_Value i -> Value i
  | Prog_Plus (v0,v1) -> Plus ((value_of_prog_value v0),(value_of_prog_value v1))
  | Prog_Minus (v0,v1) -> Minus ((value_of_prog_value v0),(value_of_prog_value v1))
  | Prog_Mult (v0,v1) -> Mult ((value_of_prog_value v0),(value_of_prog_value v1))
;;
  
let rec prop_of_prog_prop p =
  match p with
    Conj (p0,p1) -> First_order.Conj ((prop_of_prog_prop p0),(prop_of_prog_prop p1))
  | Disj (p0,p1) -> First_order.Disj ((prop_of_prog_prop p0),(prop_of_prog_prop p1))
  | Eq (v0,v1)   -> First_order.Eq (value_of_prog_value v0,value_of_prog_value v1)
  | Lt (v0,v1)   -> First_order.Lt (value_of_prog_value v0,value_of_prog_value v1)
  | Neg p0       -> First_order.Neg (prop_of_prog_prop p0)
;;
  
let rec check_affectations t =
  match t with
  Skip -> true
  | Affect (v0,t0) ->
     (match v0 with
       Prog_Variable s -> true
      | _ -> false
     )
   | Seq (t0,t1) -> ((check_affectations t0)&& (check_affectations t1))
   | If (p0,t0,t1) -> ((check_affectations t0)&& (check_affectations t1))
   | While (p0,p1,t0) -> (check_affectations t0)
;;

let rec assigned_variables t l  = 
  match t with
    Skip -> l
  | Affect ((Prog_Variable v0),t0) ->
     if (for_all (fun a-> (not (String.equal v0 a))) l) then v0::l else l
  | Affect (_,t0)-> []
  | Seq (t0,t1) -> (assigned_variables t0 (assigned_variables t1 l))
  | If (p0,t0,t1) -> (assigned_variables t0 (assigned_variables t1 l))
  | While (p0,p1,t0)-> (assigned_variables t0 l)
;;


  
let rec all_prog_variables t l =
  match t with
    Skip -> l
  | Affect (e0,e1) -> (all_value_variables (value_of_prog_value e0) (all_value_variables (value_of_prog_value e1) l))
  | Seq (t0,t1) -> (all_prog_variables t0 (all_prog_variables t1 l))
  | If (p0,t0,t1) -> (all_predicate_variables (prop_of_prog_prop p0) (all_prog_variables t0 (all_prog_variables t1 l)))
  | While (p0,p1,t0) -> (all_predicate_variables (prop_of_prog_prop p0) (all_predicate_variables  p1 (all_prog_variables t0 l)))
;;

let is_fresh v l = (for_all (fun a-> (not (String.equal v a))) l);;

let new_fresh l =
  let var = ref 0 in 
  ((while(not (is_fresh (Printf.sprintf "x%i" !var)  l))
  do var:= !var+1
    done);
   (Printf.sprintf "x%i" !var))

let assigned_assoc p =
  let l0 = ref (all_prog_variables p []) in
  let l1 = ref (assigned_variables p []) in
  let res = ref [] in 
  (while (!l1 <> [])
  do
  (match !l1 with
     [] -> ()
   | v::l2 ->
      (let v0 = (new_fresh !l0) in
       ((res :=(v,v0)::!res);
	l1 := l2;
	l0 := v0::!l0))
  )
   done);
  !res;;

;;
	  
  
let rec wlp t p =
let rec wlp_while p0 p1 t0 l q=
  match l with
    (v0,v1)::l0 -> Forall (v1, (prop_substitution v0 (Variable v1) (wlp_while p0 p1 t0 l0 q)))
  | [] -> (Conj (
	       (Impl (
		    (Conj ((prop_of_prog_prop p0),p1)),
		    (wlp t0 p1)
		  )),
	       (Impl (
		    Conj ((Neg (prop_of_prog_prop p0)),
			  p1),q))))
in
  match t with
    Skip -> p
  | Affect ((Prog_Variable v), e)-> (prop_substitution v (value_of_prog_value e) p)
  | Affect (_,e) -> p
  | Seq (t0,t1) -> (wlp t0 (wlp t1 p))
  | If (p0,t0,t1) -> (Conj ((Impl ((prop_of_prog_prop p0),(wlp t0 p))),
			  (Impl ((Neg (prop_of_prog_prop p0)),(wlp t1 p)))))
  | While (p0,p1,t0) -> Conj (p1,(wlp_while p0 p1 t0 (assigned_assoc t0) p))
;;

*)

  
(* let euclid = Skip;;			     *)

			       
(* let prog = (While ((Lt ((Prog_Value 0),(Prog_Variable "x0"))), *)
(* 				 (Eq ((Variable "X"),(Value 0))), *)
(* 				 (Seq *)
(* 				    ((Affect ((Prog_Variable "x"), *)
(* 					     (Prog_Minus ((Prog_Variable "x0"),(Prog_Value 1))))), *)
(* 				    (Affect ((Prog_Variable "z"), *)
(* 					     (Prog_Plus ((Prog_Variable "y"),(Prog_Value 1)))))) *)
(* 				 )));; *)
  
(*   if (check_affectations prog)  *)
(*   then *)
(*     (((Printf.printf "OK\n"); *)
(*       (Printf.printf "all assigned variables\n"); *)
(*       (iter (fun a->Printf.printf "%s\n" a) (assigned_variables euclid [])); *)
(*       (Printf.printf "all variables\n"); *)
(*       (iter (fun a->Printf.printf "%s\n" a) (all_prog_variables euclid [])); *)
      
(*       (if (is_fresh "Y" (all_prog_variables euclid [])) then (Printf.printf "OK is_fresh test1\n") else (Printf.printf "KO is_fresh test1\n")); *)
(*       (if (is_fresh "a" (all_prog_variables euclid [])) then (Printf.printf "KO is_fresh test2\n") else (Printf.printf "OK is_fresh test2\n")); *)
(*      (if (is_fresh "r" (all_prog_variables euclid [])) then (Printf.printf "KO is_fresh test3\n") else (Printf.printf "OK is_fresh test3\n")) *)
(*      );(Printf.printf "new fresh: %s\n" (new_fresh (all_prog_variables euclid []))); *)
(*      (let l = (assigned_assoc euclid) in *)
(*       iter (fun a -> match a with (v0,v1)-> Printf.printf "%s->%s\n" v0 v1) l *)
     

(*     )) *)
     
  
(*   else *)
(*     (Printf.printf "KO\n") *)
		       
(* ;; *)

(*   if (check_affectations (While ((Lt ((Prog_Value 0),(Prog_Variable "x"))), *)
(* 				 (Eq ((Value 0),(Value 0))), *)
(* 				 (Affect ((Prog_Value 1), (Prog_Minus ((Prog_Variable "x"),(Prog_Value 1)))))))) *)
(*   then *)
(*     (Printf.printf "KO\n") *)
(*   else *)
(*     (Printf.printf "OK\n") *)
(* ;; *)

(* Printf.printf "%s.\n" (prop_to_vernacular (wlp euclid (Eq ((Variable "a"),(Plus ((Mult ((Variable "b"),  *)
(* 		     						 (Variable "q"))),  *)
(* 		     					  (Variable "r")))))));; *)
