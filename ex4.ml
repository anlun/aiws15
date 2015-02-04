open Parity
open Cm3

type approx = t * t * t

let botApprox = (Bot, Bot, Bot)
let topApprox = (Top, Top, Top)

let approxPP a =
  let (x, y, z) = a in
  Printf.sprintf "{x:%3s, y:%3s, z:%3s}"
                 (parityPP x) (parityPP y) (parityPP z)

let updateApprox (i : inst) a =
  let (x, y, z) = a in
  match i with
  | Inc X -> (incr x, y, z) 
  | Inc Y -> (x, incr y, z)
  | Inc Z -> (x, y, incr z)
  
  | Dec X -> (decr x, y, z) 
  | Dec Y -> (x, decr y, z)
  | Dec Z -> (x, y, decr z)

  | _ -> a

let joinApprox a b =
  let (xa, ya, za) = a in
  let (xb, yb, zb) = b in
  (join xa xb, join ya yb, join za zb)

let calcApprox (l : approx list) =
  List.fold_left joinApprox botApprox l

(* -- *)
            
let updateApproxPC (i : inst) (p : pc) (a : approx) =
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

let update (p : program) (ar : approx array) : approx array =
  let newAr = Array.copy ar in
  let pcounter = ref 0 in
  List.iter (fun i ->
    pcounter := !pcounter + 1;
    let curApp  = ar.(!pcounter - 1) in
    let curInst = List.nth p (!pcounter - 1) in
    let updList = updateApproxPC curInst !pcounter curApp in

    List.iter (fun (pc, updApp) ->
      let app = newAr.(pc - 1) in
      newAr.(pc - 1) <- joinApprox app updApp
    ) updList
  ) p;
  newAr

open PrettyPrinter

let instApproxPP (n : pc) (i : inst) (a : approx) : string =
      Printf.sprintf "%3i: %-20s | %s" n (instPP i) (approxPP a)

let programApproxPP (p : program) (a : approx array) : string =
  let res = ref "" in
  for counter = Array.length a downto 1 do
    let line = instApproxPP counter (List.nth p (counter - 1)) a.(counter - 1) in
    res := line ^ "\n" ^ !res
  done;
  !res

open Parser

let pr_ex3 =
  "inc y
   zero y 1 else 1 
   stop y"


let () =
  let pr1 = fst (List.hd (progP pr_ex3)) in
  let oldApp = ref (Array.make (List.length pr1) botApprox) in
  let app    = ref (Array.copy !oldApp) in
  !app.(0) <- (Top, Even, Even);
  while app <> oldApp do
    Printf.printf "%s\n\n" (programApproxPP pr1 !app);
    let newApp = update pr1 !app in
    oldApp := !app;
    app    := newApp
  done

