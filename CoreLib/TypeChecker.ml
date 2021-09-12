open MicroC;;
open Env;;

(* for now, forbids declarations of function variables in While and if... makes sense *)
(* TODO : raise exceptions for more details on type errors*)


let rec update_type_env (env : type_environment)(i:instruction) = 
  match i with
  | Declare (tt,id,_) -> (declare env id tt)
  | Seq (i0,i1)       -> (update_type_env (update_type_env env i0) i1)
  | _                 -> env
;;

let rec type_check_instruction (env:type_environment)(i:instruction)(tt:ttype) : bool = 
  match i with
  | Skip e0            -> type_check_expression env e0 tt
  | Declare (tt,_,e0) -> type_check_expression env e0 tt
  | Seq (i0,i1)        -> (type_check_instruction env i0 tt) && (let env' = update_type_env env i0 in (type_check_instruction env' i1 tt))
  | Assign (id,e0)     -> type_check_expression env e0 (get env id) 
  | If (c0,i0,i1)      -> (type_check_condition env c0) && (type_check_instruction env i0 tt) && (type_check_instruction env i1 tt) 
  | While (c0,i0)      -> (type_check_condition env c0) && (type_check_instruction env i0 tt)
  | Call (id0,l0)      -> (match (get env id0) with | FunctionT (l1,tt0) -> tt0=tt && (type_check_list_expression env l0 l1)  | _ -> false)
  | Function _        -> false
  | Return e0          -> (type_check_expression env e0 tt)
  and type_check_expression      (env:type_environment)(e:expression)(tt:ttype)  : bool =
match e with 
| Variable v      -> tt = get env v 
| Value    v      -> 
    (match (tt,v) with 
      | (Int, Tint _) -> true
      | (Float, Tfloat _) -> true
      | (FunctionT (ttlist,ttret), Tfun (largs,ttret',def)) -> 
        (ttlist = (List.map snd largs)) &&
        (ttret = ttret') && 
        (type_check_instruction (declare_all (open_scope env) (List.map fst largs) ttlist) def ttret)
      | (_,_) -> false
    )
| BinOp (_,e0,e1) -> (type_check_expression env e0 tt) &&  (type_check_expression env e1 tt)
| Callexp i0      -> (type_check_instruction env i0 tt)
and type_check_condition        (env:type_environment)(e:condition) : bool =
(match e with 
| TRUE  -> true
| FALSE -> true
| Neg c0 -> type_check_condition env c0 
| CondBin (_,c0,c1) -> (type_check_condition env c0)  && (type_check_condition env c1)
| Compare (_,e0,e1) -> 
    (((type_check_expression env e0 Int) && (type_check_expression env e1 Int)) ||
    ((type_check_expression env e0 Float) && (type_check_expression env e1 Float))
    )
)
and type_check_list_expression env lexp lttype =
match (lexp,lttype) with
| (e0::lexp',tt0::lttype') -> (type_check_expression env e0 tt0) && (type_check_list_expression env lexp' lttype')
| ([],[]) -> true
| _       -> false  
  ;;
