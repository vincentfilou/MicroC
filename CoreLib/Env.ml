(* TODO : tests + dune for building *)

open MicroC;;

exception Undefined_Variable of string;;
exception Undefined_Function of string;;
exception Already_Defined_Variable of string;;
exception Already_Defined_Function of string;;
exception No_Open_Scope;;

(* 
  modify get to return an env_elt (Variable -> functions)
  modify declare/function_declare : no redefinition of vars with different types ? 
  env_elt -> typed value + Scope?

  dynamic typing : when set, check that the type of the variable is not changed ? 

  type of : TypedValue -> Type ?

  env_elt : 
  Scope -> env_elt
  Value -> string -> TypedValue -> env_elt 
  
*)

(*

type ttype =
| Int 
| Function of ((ttype list)*ttype)
;;

type typed_value =
| Tint of int 
| Tfun of ((string*ttype) list)*ttype*instruction 
;;

let ttype_of_typed_value v = 
  match v with 
  | Tint _ -> Int
  | Tfun (lt,tt,_) -> Function ((map (fun x ->snd x) lt)*tt)
;;

type 'a elt =
| Scope 
| Elt of (string*'a)
;;

type 'a environment = ('a elt) list;;

type eval_environment = (ttyped_value environment);;
type type_envrionment = (ttype environment);;

let filter (e:environment)(tt:string)

*)

type env_elt = 
| Scope 
| Int of (string*int)
| Function of (string*(string list)*instruction)
;; 

type environment = env_elt list;;

let empty = [];;

let rec is_declared_in_scope (e:environment)(id:string) =
match e with 
| [] -> false
| Scope :: _          -> false
| Function (_,_,_)::e0 -> is_declared_in_scope e0 id 
| Int (id0,_)::e0    -> if id = id0 then true else is_declared_in_scope e0 id
;;

let rec function_is_declared_in_scope (e:environment)(id:string) = 
match e with 
| [] -> false
| Scope :: _          -> false
| Function (id0,_,_)::e0 -> if id = id0 then true else function_is_declared_in_scope e0 id
| Int (_,_)::e0    -> function_is_declared_in_scope e0 id 
;;

(* declare the variable id in the current scope *)

let declare (e:environment)(id:string)(v:int) =
  if is_declared_in_scope e id then raise (Already_Defined_Variable id)
  else Int (id,v)::e
;;

(* declare the function id in the current scope *)

let function_declare (e:environment)(id:string)(args:string list)(def:instruction) = 
  if function_is_declared_in_scope e id then raise (Already_Defined_Function id)
  else (Function (id,args,def))::e;;

(* set the value of id in the last scope in which it as been declared *)

let rec set (e:environment)(id:string)(v:int) = 
  match e with 
  | [] -> raise (Undefined_Variable id)
  | Scope::e0 -> Scope::(set e0 id v)
  | Function (id0,args,def)::e0 ->  Function (id0,args,def)::(set e0 id v) 
  | Int (id0,v0)::e0 -> if id0 = id then (Int (id0,v)::e0) else (Int (id0,v0))::(set e0 id v)
;;

(* get the Int of id in the last scope in which it as been declared *)

let rec get (e:environment)(id:string) =
  match e with 
  | [] -> raise (Undefined_Variable id)
  | Scope::e0 -> get e0 id
  | Function (_,_,_)::e0 -> get e0 id 
  | Int (id0,v0)::e0 -> if id0 = id then v0 else (get e0 id)
;;

let rec function_get (e:environment)(id:string) = 
match e with 
  | [] -> raise (Undefined_Function id)
  | Scope::e0 -> function_get e0 id
  | Function (id0,args,def)::e0 -> if id0 = id then (args,def) else function_get e0 id 
  | Int (_,_)::e0 -> function_get e0 id
;; 

(* add marker *)
let open_scope  (e:environment) = Scope::e;;

(* pop until marker *)
let rec close_scope (e:environment) = 
match e with 
| [] -> raise (No_Open_Scope)
| Scope::e0 -> e0
| _ :: e0   -> close_scope e0
;;

let env_elt_to_string (x:env_elt) =
  match x with 
  | Scope      -> "Scope"
  | Int (id,v) -> String.concat "" [id;"=";string_of_int v]
  | Function (id,_,_) -> String.concat "" ["Function ";id]
;;

let rec declare_all (e:environment)(ids: string list)(values : int list) =
  match (ids,values) with 
  | (id0::ids0,v0::values0) -> declare_all (declare e id0 v0) ids0 values0
  | ([],_) -> e
  | (id0::_,[]) -> raise (Undefined_Variable id0)
;;

let included (e0:environment)(e1:environment) = 
  List.fold_right (fun x b-> if b then (List.mem x e1) else false ) e0 true;;

let equal e0 e1 = included e0 e1 && included e1 e0;;

let environment_to_string (e:environment) =
  String.concat "\n" (List.map env_elt_to_string e)
;;