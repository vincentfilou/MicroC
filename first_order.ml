open List;;
  
type value =
    Variable of string
  | Value of int
  | Plus of value*value
  | Minus of value*value
  | Mult of value*value
;;

type prop = 
    Neg of prop
  | Conj of prop*prop
  | Disj of prop*prop
  | Impl of prop*prop
  | Eq of value*value
  | Lt of value*value
  | Forall of string*prop
  | Exist of string*prop;;

let rec bound a l =
  match a with
    Forall (x,y) -> cons x (bound y l) 
  | Exist (x,y) -> cons x (bound y l)
  | _ -> l;;


let rec val_substitution x e v =
  match v with
   Variable s-> if (String.equal s x) then e else Variable s
  | Value i -> Value i
  | Plus (i,j) -> Plus ((val_substitution x e i),(val_substitution x e j))
  | Minus (i,j) -> Minus ((val_substitution x e i),(val_substitution x e j))
  | Mult (i,j) -> Mult ((val_substitution x e i),(val_substitution x e j))
;;
		     
		   

let rec prop_substitution x e p=
  match p with
    Forall (y,p0) ->if (not (String.equal x y))
		    then Forall (y,prop_substitution x e p0)
		    else Forall (y,p0)
  | Exist (y,p0) ->if (not (String.equal x y))
		   then Exist (y,prop_substitution x e p0)
		   else Exist (y,p0)
  | Impl (a,b) -> Impl ((prop_substitution x e a),(prop_substitution x e b))
  | Neg a -> Neg (prop_substitution x e a)
  | Conj (a,b) -> Conj ((prop_substitution x e a),(prop_substitution x e b))
  | Disj (a,b) -> Disj ((prop_substitution x e a),(prop_substitution x e b))
  | Eq (a,b) -> Eq ((val_substitution x e a),(val_substitution x e b))
  | Lt (a,b) -> Lt ((val_substitution x e a),(val_substitution x e b))
;;

let rec all_value_variables v l =
match v with
   Variable s-> if (for_all (fun a-> (not (String.equal s a))) l) then s::l else l
  | Value i -> l
  | Plus (a,b) -> (all_value_variables a (all_value_variables b l))
  | Minus (a,b) -> (all_value_variables a (all_value_variables b l))
  | Mult (a,b) -> (all_value_variables a (all_value_variables b l))
;;
  
  
let rec all_predicate_variables p l=
  match p with
    Forall (y,p0) -> y::(all_predicate_variables p0 l)
  | Exist (y,p0) ->y::(all_predicate_variables p0 l)
  | Impl (a,b) -> (all_predicate_variables a (all_predicate_variables b l))
  | Neg a -> (all_predicate_variables a l)
  | Conj (a,b) -> (all_predicate_variables a (all_predicate_variables b l))
  | Disj (a,b) -> (all_predicate_variables a (all_predicate_variables b l))
  | Eq (a,b) -> (all_value_variables a (all_value_variables b l))
  | Lt (a,b) -> (all_value_variables a (all_value_variables b l))
;;

  
let rec val_to_vernacular a =
  match a with
   Variable v -> v
  | Value v   -> (Printf.sprintf "%i" v)
  | Plus (a,b) -> (Printf.sprintf "(%s + %s)" (val_to_vernacular a) (val_to_vernacular b))
  | Minus (a,b) -> (Printf.sprintf "(%s - %s)" (val_to_vernacular a) (val_to_vernacular b))
  | Mult (a,b) -> (Printf.sprintf "(mult %s %s)" (val_to_vernacular a) (val_to_vernacular b))
;;

let rec prop_to_vernacular p =
  match p with
   Forall (y,p0) -> (Printf.sprintf "forall %s:nat, %s" y (prop_to_vernacular p0))
  | Exist (y,p0) -> (Printf.sprintf "exists %s:nat, %s" y (prop_to_vernacular p0))
  | Impl (a,b) -> (Printf.sprintf "(%s -> %s)" (prop_to_vernacular a) (prop_to_vernacular b))
  | Neg a -> (Printf.sprintf "(not %s)" (prop_to_vernacular a) )
  | Conj (a,b) -> (Printf.sprintf "(%s /\\ %s)" (prop_to_vernacular a) (prop_to_vernacular b))
  | Disj (a,b) -> (Printf.sprintf "(%s \\/ %s)" (prop_to_vernacular a) (prop_to_vernacular b))
  | Eq (a,b) -> (Printf.sprintf "(%s = %s)" (val_to_vernacular a) (val_to_vernacular b))
  | Lt (a,b) -> (Printf.sprintf "(%s < %s)" (val_to_vernacular a) (val_to_vernacular b))
;;
    
		   
  (* Printf.printf "%s.\n" (prop_to_vernacular (prop_substitution "y" (Plus ((Variable "x"), Value 1)) (Forall ("x",(Disj ((Eq ((Value 0),(Variable "x"))),(Lt ((Value 0),(Variable "y")))))))));; *)
