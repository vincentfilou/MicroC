open Lib;;
open Lib.MicroC;;
open Lib.Env;;
  
let debug_hook x =
  print_string "\n===================\n";
  print_string "\n=========ENV=======\n";
  print_string (environment_to_string (fst x));
  print_string "\n=========PROG======\n";
  print_string (instruction_to_string (snd x));
  (match (read_line ()) with | _ -> ());()
;;

let main =
  let (f,x) = (if Sys.argv.(1) = "-d" then (debug_hook,2) else ((fun _ -> ()),1)) in  
  let prog = (empty,(Loader.load_file (Sys.argv.(x)))) in
  let small_step_result = SmallStep.eval_loop f prog in
  let big_step_result   = BigStep.eval prog in 
  print_string "\n==========Check===========\n";
  (if (equal (fst small_step_result)
            (fst big_step_result))
    then (print_string "OK") else (print_string "KO"));
    print_string "\n==========Result==========\n";
    print_string (environment_to_string (fst big_step_result))
;;

main;;
		      
