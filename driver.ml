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
     
let () =
  let pr1 = fst (List.hd (progP pr_ex3)) in
  (*let module M = AbstractInterpreter(IntervalState) in*)
  let module M = AbstractInterpreter(ParityState) in
  M.loop pr1
