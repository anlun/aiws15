open Cm3
open Interval
open Parity
open AbstractInterpreter
open Parser

let pr_ex3 =
  "inc y
   zero y 1 else 1 
   stop"

let pr_ex4 =
"inc z
 zero z 3 else 4
 inc y
 dec z
 stop"

let pr_ex5 = 
"inc y
 dec y
 inc y
 dec z
 zero x 3 else 6
 inc z
 dec x
 zero x 6 else 9
 dec z
 zero z 8 else 11
 stop"

     
let () =
  let pr1 = fst (List.hd (progP pr_ex5)) in
  let module M = AbstractInterpreter(IntervalState) in
  (*let module M = AbstractInterpreter(ParityState) in*)
  M.loop pr1
