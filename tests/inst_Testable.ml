open Alcotest;;
open Lib;;


module TestableInstruction : TESTABLE = 
struct
  type t = MicroC.instruction
  let pp = Lib.Pretty_print.pp
  let equal x y = x = y
end
