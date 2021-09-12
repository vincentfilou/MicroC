(* TODO : tests + dune for building *)

exception Undefined_Variable of string;;
exception Undefined_Function of string;;

exception Already_Defined_Variable of string;;
exception Already_Defined_Function of string;;
exception No_Open_Scope;;
exception Type_Exception;;


type 'a elt =
| Scope 
| Elt of (string*'a)
;;

type 'a environment = ('a elt) list;;

let empty : 'a environment = [];;

let rec is_declared_in_scope (e:'a environment)(id:string) =
match e with 
| [] -> false
| Scope :: _  -> false
| Elt (id0,_)::e0 -> if id0 = id then true else is_declared_in_scope e0 id 
;;

(* declare the variable id in the current scope *)

let declare (e:'a environment)(id:string)(v:'a) : 'a environment =
  if is_declared_in_scope e id then raise (Already_Defined_Variable id)
  else Elt (id,v)::e
;;

(* set the value of id in the last scope in which it as been declared *)

let rec set (e:'a environment)(id:string)(v : 'a ) : 'a environment = 
  match e with 
  | [] -> raise (Undefined_Variable id)
  | Scope::e0 -> Scope::(set e0 id v)
  | Elt (id0,v0)::e0 -> 
      if id0 = id then Elt (id0,v)::e0 else  Elt(id0,v0)::(set e0 id v)
;;

(* get the value of id in the last scope in which it as been declared *)

let rec get (e:'a environment)(id:string) : 'a =
  match e with 
  | [] -> raise (Undefined_Variable id)
  | Scope::e0   -> get e0 id
  | Elt (id0,v)::e0 -> if  id0 = id then v else (get e0 id)
;;

(* add marker *)

let open_scope  (e:'a environment) = Scope::e;;

(* pop until marker *)

let rec close_scope (e:'a environment) : 'a environment= 
match e with 
| []        -> raise (No_Open_Scope)
| Scope::e0 -> e0
| _ :: e0   -> close_scope e0
;;

let string_of_environment (f : 'a -> string)(e:'a environment) : string =
  let string_of_elt x = match x with | Scope -> "Scope" | Elt (id,v) -> id^":"^(f v) in 
  String.concat ";" (List.map string_of_elt e)
;;

let rec declare_all (e:'a environment)(ids: string list)(values : 'a list) : 'a environment=
  match (ids,values) with 
  | (id0::ids0,v0::values0) -> declare_all (declare e id0 v0) ids0 values0
  | ([],_) -> e
  | (id0::_,[]) -> raise (Undefined_Variable id0)
;;

let included (e0:'a environment)(e1:'a environment) = 
  List.fold_right (fun x b-> if b then (List.mem x e1) else false ) e0 true;;

let equal e0 e1 = included e0 e1 && included e1 e0;;
