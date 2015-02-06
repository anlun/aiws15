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
 dec z
 zero x 1 else 4
 inc z
 dec x
 zero x 4 else 7
 dec z
 zero z 6 else 9
 stop"

let pr_ex6 = 
"inc y
 dec z
 zero x 4 else 1
 inc z
 dec x
 zero x 7 else 4
 dec z
 zero z 9 else 6
 stop"

     
let () =
  let pr1 = fst (List.hd (progP pr_ex6)) in
  let module M = AbstractInterpreter (IntervalState) (ForwardAnalysis) in
  (*let module M = AbstractInterpreter(ParityState) in*)
  M.loop pr1
