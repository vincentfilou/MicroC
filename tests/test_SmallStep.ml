open Lib.Env;;
open Lib.SmallStep;;
open Alcotest;;

open Test_Progs;;

(* TODO : factoriser *)


let test_eval_0 () =
  Alcotest.(check int) "same int" 10 (let x = (simple_eval_loop (empty, test_prog_0)) in (get (fst x) "i" ));;

let test_eval_1 () =
  Alcotest.(check int) "same int" 10 (let x = (simple_eval_loop (empty, test_prog_1)) in (get (fst x) "x" ));;

let test_eval_2 () =
  Alcotest.(check int) "same int" 11 (let x = (simple_eval_loop (empty, test_prog_2)) in (get (fst x) "x" ));;

let test_eval_3 () =
  Alcotest.(check int) "same int" 2 (let x = (simple_eval_loop (empty, test_prog_3)) in (get (fst x) "x" ));;
      
let test_eval_4 () =
  Alcotest.(check (list string) "same" ["a"] (let x = (simple_eval_loop (empty, test_prog_4)) in (fst (function_get (fst x) "f" ))));;

let test_eval_5 () =
    Alcotest.(check int) "same int" 100 (let x = (simple_eval_loop (empty, test_prog_5)) in (get (fst x) "x" ));;    


let tests = [ 
test_case "get test_prog_0"     `Quick test_eval_0;
test_case "get test_prog_1"     `Quick test_eval_1;
test_case "get test_prog_2"     `Quick test_eval_2;
test_case "get test_prog_3"     `Quick test_eval_3;
test_case "get test_prog_4"     `Quick test_eval_4;
test_case "get test_prog_5"     `Quick test_eval_5;
 ];;

(* let () =
  let open Alcotest in
  run "Eval" [
      "Env-tests" , Test_Env.env_test ;
      "Eval-get", [ test_case "get test_prog_0"     `Quick test_eval_0;
                    test_case "get test_prog_1"     `Quick test_eval_1;
                    test_case "get test_prog_2"     `Quick test_eval_2;
                    test_case "get test_prog_3"     `Quick test_eval_3;
                    test_case "get test_prog_4"     `Quick test_eval_4;
                    test_case "get test_prog_5"     `Quick test_eval_5;
                     ];
        
  ]
;; *)


