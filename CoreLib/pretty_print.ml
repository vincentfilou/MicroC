open MicroC;;

let pp (out:Format.formatter)(x:instruction) = Format.pp_print_string out (instruction_to_string x);;