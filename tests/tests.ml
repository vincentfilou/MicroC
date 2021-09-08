
 let open Alcotest in
 run "Eval" [
     "Env tests" , Test_Env.tests ;
     "SmallStep tests", Test_SmallStep.tests ;
     "Parser tests", Test_Parser.tests;
     "BigStep tests", Test_BigStep.tests;
 ];;