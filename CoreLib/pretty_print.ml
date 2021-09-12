(* open MicroC;;




let pp (out:Format.formatter)(x:instruction) = Format.pp_print_string out (string_of_instruction x);; *)
open MicroC;;
open String;;
open Env;;

(* TODO *)



let rec string_of_instruction i0 = 
  (match i0 with 
  | Skip exp -> concat " " ["Skip"; string_of_expression exp]
  | Declare (tt,id,exp) -> concat " " ["Declare ";(string_of_type tt);":";id; string_of_expression exp]
  | Seq (i0,i1) -> concat "" ["(";string_of_instruction i0;";\n";string_of_instruction i1;")"]
  | Assign (id,exp) -> concat "" [id;":=";string_of_expression exp]
  | If (c0,i0,i1) -> concat "" ["if (";string_of_condition c0;")\n{";string_of_instruction i0;"\n}else{\n";string_of_instruction i1;"\n}"]
  | While (c0,i0) -> concat "" ["While (";string_of_condition c0;"){\n";string_of_instruction i0;"\n}"]
  | Call  (id,largs) -> concat "" [id;"("; (concat "," (List.map string_of_expression largs));")"]
  | Function i0      -> concat "" ["Function (";string_of_instruction i0;")"] 
  | Return exp       -> concat "" ["Return (";string_of_expression exp;")"]
  )
  and string_of_expression exp =
  (match exp with
  | Variable id -> id
  | Value v     -> (string_of_typed_value v)
  | BinOp (op,exp0,exp1) ->
      let op_to_string op = match op with | Plus -> "+" | Minus -> "-" | Mult -> "*" in
    (concat "" [string_of_expression exp0;op_to_string op; string_of_expression exp1])
  | Callexp i0 -> string_of_instruction i0
  )
  and string_of_condition cond =
  (match cond with
  | TRUE -> "TRUE"
  | FALSE -> "FALSE"
  | Neg cond0 -> concat "" ["not(";string_of_condition cond0;")"]
  | CondBin (And,cond0,cond1) -> concat "" ["(";string_of_condition cond0;" & ";string_of_condition cond1]
  | CondBin (Or,cond0,cond1) -> concat "" ["(";string_of_condition cond0;" | ";string_of_condition cond1]
  | Compare (Eq,exp0,exp1)   -> concat "" [ string_of_expression exp0;" == ";string_of_expression exp1]
  | Compare (Lt,exp0,exp1)   -> concat "" [ string_of_expression exp0;" < ";string_of_expression exp1]
)
and string_of_typed_value (tv:typed_value) : string = 
  (match tv with 
  | Tint v   -> (string_of_int v)
  | Tfloat v -> (string_of_float v)
  | Tfun (_,_,_) -> "Function"
  )
and string_of_type (tt:ttype) : string = 
  match tt with 
  | Int -> "int"
  | Float -> "float"
  | FunctionT (l,tt')-> (String.concat "->" (List.map string_of_type l))^"->"^(string_of_type tt')
  ;;

  let string_of_eval_environment (e:eval_environment) = string_of_environment string_of_typed_value e;;

  let string_of_type_environment (e:type_environment) = string_of_environment string_of_type e;;
  