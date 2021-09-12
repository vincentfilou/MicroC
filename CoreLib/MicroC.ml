type bin_op = | Plus | Minus | Mult;;
type comp_op = | Lt | Eq;;
type cond_bin_op = | And | Or;;

type ttype =
  | Int 
  | Float
  | FunctionT of ((ttype list)*ttype)
;;

exception Empty_Function_Type;;

let rec function_type_from_list (l:ttype list)(acc: (ttype list)) = 
  match l with 
  | tt :: [] -> FunctionT (acc,tt)
  | tt :: l0 -> function_type_from_list l0 (List.concat [acc;[tt]])
  | _ -> raise Empty_Function_Type
;;

type typed_value =
  | Tint   of int 
  | Tfloat of float
  | Tfun   of ((string*ttype) list)*ttype*instruction 
and expression =
  | Variable of string
  | Value    of typed_value
  | BinOp    of bin_op*expression*expression
  | Callexp  of instruction
and condition = 
  | TRUE
  | FALSE 
  | Neg      of condition
  | CondBin  of cond_bin_op*condition*condition
  | Compare  of comp_op*expression*expression
and instruction =
  | Skip     of expression
  | Declare  of ttype*string*expression
  | Seq      of instruction*instruction
  | Assign   of string*expression
  | If       of condition*instruction*instruction
  | While    of condition*instruction
  | Call     of string*(expression list)
  | Function of instruction
  | Return   of expression
;;

let ttype_of_typed_value v = 
  match v with 
  | Tint _         -> Int
  | Tfloat _       -> Float
  | Tfun (lt,tt,_) -> FunctionT ((List.map (fun x ->snd x) lt),tt)
;;

open Env;;

type eval_environment = (typed_value environment);;
type type_environment = (ttype environment);;

let eval_binop (op: bin_op)(v0 :typed_value)(v1: typed_value) : typed_value =
  match (v0,v1) with
  | (Tint v0, Tint v1) ->  (match op with | Plus -> Tint (v0+v1) | Minus -> Tint (v0-v1) | Mult -> Tint (v0*v1))
  | (Tfloat v0, Tfloat v1) -> (match op with | Plus -> Tfloat (Float.add v0 v1) | Minus -> Tfloat (Float.sub v0 v1) | Mult -> Tfloat (Float.mul v0 v1))
  | (_,_) -> raise Type_Exception
;;


(* 
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
 *)
