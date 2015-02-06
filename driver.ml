open Cm3
open Interval

module AbstractInterpreter = functor (M : AnalysisType) ->
struct
  open M

  let updateApproxPC (i : inst) (p : pc) (a : t * t * t) =
    let (x, y, z) = a in
    match i with
    | Inc X -> [p + 1, (incr x, y, z)] 
    | Inc Y -> [p + 1, (x, incr y, z)]
    | Inc Z -> [p + 1, (x, y, incr z)]
  
    | Dec X -> [p + 1, (decr x, y, z)]
    | Dec Y -> [p + 1, (x, decr y, z)]
    | Dec Z -> [p + 1, (x, y, decr z)]

    | Zero (X, m, n) -> [m, (iszero x, y, z); n, (notzero x, y, z)]
    | Zero (Y, m, n) -> [m, (x, iszero y, z); n, (x, notzero y, z)]
    | Zero (Z, m, n) -> [m, (x, y, iszero z); n, (x, y, notzero z)]

    | Stop -> []

  type approx = (t * t * t) array
                  
  let narrowApprox (a : approx) (b : approx) =
    let res = Array.copy a in
    for i = 0 to Array.length a - 1 do
      res.(i) <- narrow a.(i) b.(i) 
    done;
    res
  
  let widenApprox (a : approx) (b : approx) =
    let res = Array.copy a in
    for i = 0 to Array.length a - 1 do
      res.(i) <- widen a.(i) b.(i) 
    done;
    res

  let updateF f (p : program) (ar : (t * t * t) array) : (t * t * t) array =
    let newAr = Array.copy ar in
    let pcounter = ref 0 in
    List.iter (fun i ->
      pcounter := !pcounter + 1;
      let curApp  = ar.(!pcounter - 1) in
      let updList = updateApproxPC i !pcounter curApp in
      
      (*
      List.iter (fun (pc, updApp) ->
                 Printf.printf "%i | %s\n" pc (triplePP updApp)
                ) updList;
      *)
      List.iter (fun (pc, updApp) ->
        let app = newAr.(pc - 1) in
        if not (containsBot updApp) then
          (*newAr.(pc - 1) <- joinTriple app updApp*)
          newAr.(pc - 1) <- f app updApp
          (*;
          Printf.printf "%i\n-----\n%s\n" pc (triplePP app);
          Printf.printf "%s\n"(triplePP updApp);
          Printf.printf "==\n%s\n"(triplePP newAr.(pc - 1))
           *)
      ) updList
    ) p;
    newAr
  
  let update = updateF widen

  open PrettyPrinter
  let instApproxPP (n : pc) (i : inst) (a : t * t * t) : string =
    Printf.sprintf "%3i: %-20s | %s" n (instPP i) (triplePP a)

  let programApproxPP (p : program) (a : (t * t * t) array) : string =
    let res = ref "" in
    for counter = Array.length a downto 1 do
      let line =
        instApproxPP counter (List.nth p (counter - 1)) a.(counter - 1)
      in
      res := line ^ "\n" ^ !res
    done;
    !res

  let loop pr =
    let fsharpWiden  = updateF widen  pr in
    let fsharpNarrow = updateF narrow pr in
    let printer = programApproxPP pr in
    let oldApp = ref (Array.make (List.length pr) botTriple) in
    let app    = ref (Array.copy !oldApp) in
    !app.(0) <- initTriple;
    let iter f =
      while app <> oldApp do
        Printf.printf "%s\n" (printer !app);
        let newApp = f !app in 
        oldApp := !app;
        app    := newApp
      done
    in
    Printf.printf "Widening:\n";
    iter fsharpWiden;
    Printf.printf "Narrowing:\n";
    oldApp := Array.make (List.length pr) botTriple; 
    iter fsharpNarrow
end

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
  let module M = AbstractInterpreter(IntervalState) in
  M.loop pr1
