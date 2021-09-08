open Lib.Env;;
open Alcotest;;


let test_env_get_0 () =
Alcotest.(check int) "same int" 1 (get (declare empty "x" 1) "x")

let test_env_get_1 () =
  Alcotest.(check int) "same int" 2 (get (set (declare empty "x" 1) "x" 2) "x")

let test_env_get_2 () =
    Alcotest.(check int) "same int" 2 (get (declare (open_scope (declare empty "x" 1)) "x" 2) "x")

let test_env_get_3 () = 
  Alcotest.(check_raises) "same" (Undefined_Variable "x") (fun ()->match (get empty "x") with _ ->())

let test_env_get_4 () =
  Alcotest.(check int) "same int" 1 (get (close_scope (declare (open_scope (declare empty "x" 1)) "x" 2)) "x")

let test_env_getfun_0 () =
  Alcotest.(check_raises) "same" (Undefined_Function "f") (fun ()->match (function_get empty "f") with _ ->())

let test_env_getfun_1 () =
  Alcotest.(check (list string)) "same" ["a"] (fst (function_get (function_declare empty "f" ["a"] (Return (Value 0))) "f"))


let tests = [ test_case "get declared"     `Quick test_env_get_0;
                   test_case "get set"          `Quick test_env_get_1;  
                   test_case "get new scope"    `Quick test_env_get_2; 
                   test_case "get empty env"    `Quick test_env_get_3;
                   test_case "get close scope"  `Quick test_env_get_4;
                   test_case "get fun empty  "  `Quick test_env_getfun_0; 
                   test_case "get fun declared" `Quick test_env_getfun_1;
                   ]
;;