open Arg;;
open First_order;;
open Gcl;;

let output = ref "";;
let file = ref "";;
let set_file f = (file := f);;
let set_output f = (output := f);;

  
let run a b =
  try 
    let lexbuf = (Lexing.from_channel a) in
    while true do
      
      let result = Parser.main Lexer.token lexbuf in
      (match result with
	 (p0,t,p1) -> Printf.fprintf b "%s.\n" (prop_to_vernacular (Impl (p0,(wlp t p1)))));
      
      flush b
    done;
  with e ->()
;;


  
let main =
  begin
    let speclist =[("-f", Arg.String (set_file),"file to parse");
		   ("-o", Arg.String (set_output), "output")
		  ] in
    let usage_msg = "compute WLP" in
    let out = ref stdout in
    let inp = ref stdin in

    Arg.parse speclist (fun a->()) usage_msg;
    (

      (if (not (String.equal !file ""))
      then     
	inp := (open_in !file) 
      );
      (if (not (String.equal !output ""))
      then
	out := open_out !output
      );
      (try
	  (run !inp !out);close_in !inp;close_out !out
	with e-> (close_in_noerr !inp;close_out_noerr !out);raise e
      )
    );exit 0
  end
		      
  
let () = main
  
