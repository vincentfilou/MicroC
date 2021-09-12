open Lib;;
open Lib.Env;;
open Lib.Pretty_print;;
  
let debug_hook x =
  print_string "\n===================\n";
  print_string "\n=========ENV=======\n";
  print_string (string_of_eval_environment (fst x));
  print_string "\n=========PROG======\n";
  print_string (string_of_instruction (snd x));
  (match (read_line ()) with | _ -> ());()
;;

let main =
  let (f,x) = (if Sys.argv.(1) = "-d" then (debug_hook,2) else ((fun _ -> ()),1)) in  
  let prog = (empty,(Loader.load_file (Sys.argv.(x)))) in
  (if (TypeChecker.type_check_instruction empty (snd prog) Int) then print_string "\nTYPE OK\n" else print_string "\nTYPE KO\n");
  let small_step_result = SmallStep.eval_loop f prog in
  let big_step_result   = BigStep.eval prog in 
  print_string "\n==========Check===========\n";
  (if (equal (fst small_step_result)
            (fst big_step_result))
    then (print_string "OK") else (print_string "KO"));
    print_string "\n==========Result==========\n";
    print_string (string_of_eval_environment (fst big_step_result))
;;

main;;
		      
