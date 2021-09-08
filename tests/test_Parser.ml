open Alcotest;;
open Lib;;
open Test_Progs;;

let tinstruction = testable Lib.Pretty_print.pp (fun x y -> x=y);;

let test_parse_0 () =
  Alcotest.(check tinstruction) "same" test_prog_0 (Loader.load_file "test_inputs/test_prog_0.micro");;

let test_parse_1 () =
    Alcotest.(check tinstruction) "same" test_prog_1 (Loader.load_file "test_inputs/test_prog_1.micro");;

let tests = [ 
  test_case "parse prog_0"     `Quick test_parse_0;
  test_case "parse prog_1"     `Quick test_parse_1;


];;

 