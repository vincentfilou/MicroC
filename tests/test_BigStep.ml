open Alcotest;;
open Lib;;

let pp (out:Format.formatter)(x:Env.environment) = Format.pp_print_string out (Env.environment_to_string x);;

let tenv = testable pp Lib.Env.equal;;

let test_eval_0 () = 
  let prog = (Env.empty,Lib.Loader.load_file "test_inputs/euclid.micro") in
  let e0 = (fst (SmallStep.simple_eval_loop prog)) in
  let e1 = (fst (BigStep.eval prog)) in 
  Alcotest.(check tenv) "same" e0 e1
;;

let test_eval_1 () = 
  let prog= (Env.empty,Lib.Loader.load_file "test_inputs/global.micro") in
  let e0 = (fst (SmallStep.simple_eval_loop prog)) in
  let e1 = (fst (BigStep.eval prog)) in 
  Alcotest.(check tenv) "same" e0 e1;
  Alcotest.(check int)  "same" 10 (Env.get e1 "x")
;;

let test_eval_2 () = 
  let prog= (Env.empty,Lib.Loader.load_file "test_inputs/fun_args.micro") in
  let e0 = (fst (SmallStep.simple_eval_loop prog)) in
  let e1 = (fst (BigStep.eval prog)) in 
  Alcotest.(check tenv) "same" e0 e1;
  Alcotest.(check int)  "same" 9 (Env.get e1 "x")
;;

let tests = [ test_case "test euclid" `Quick test_eval_0;
              test_case "test globals" `Quick test_eval_1;
              test_case "test function arguments" `Quick test_eval_2;
];;
