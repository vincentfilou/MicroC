open Lib.MicroC;;

let test_prog_0 = (Declare ("i",(Value 10)));;
let test_prog_1 = (Seq (Declare ("x",(Value 1)),(Assign ("x", (Value 10)))));;
let test_prog_2 = (Seq (Declare ("x",(Value 1)),(Seq ((Assign ("x", Value 10)),(Assign ("x",(BinOp (Plus, (Variable "x"),(Value 1)))))))));;
let test_prog_3 = (Seq ((Declare ("x",( Value 10))), (While ((Compare (Lt, Value 2,(Variable "x"))),(Assign (("x"), (BinOp (Minus, (Variable "x"),(Value 1)))))))));;
let test_prog_4 = (Seq ((Declare ("x",Value 0)),(Seq ((Defun ("f",["a"],(Return (Value 100)))),Skip (Value 0)))));;
let test_prog_5 = (Seq ((Declare ("x",Value 0)),(Seq ((Defun ("f",[],(Seq ((Return (Value 100)),Return (Value 0))))),(Assign ("x", Callexp (Call ("f",[]))))))));;
