module Interval =
struct
  type t = Bot | NegInf of int | Fin of int * int | PosInf of int | Top

  let pp (i : t) =
    match i with
    | Bot -> "O"
    | Top -> "Z"
    | NegInf   x -> Printf.sprintf "(-inf, %4i)" x
    | PosInf   x -> Printf.sprintf "(%4i, +inf)" x
    | Fin (x, y) -> Printf.sprintf "(%4i, %4i)" x y

  let bot : t = Bot

  let leq a b =
    match a, b with
    | Bot, _ -> true
    | _, Top -> true
    | NegInf     x, NegInf y     -> x <= y
    | Fin   (_, x), NegInf y     -> x <= y
    | Fin (xa, ya), Fin (xb, yb) -> xa >= xb && ya <= yb
    | Fin   (x, _), PosInf y     -> x >= y
    | PosInf     x, PosInf y     -> x >= y
    | _ -> false
  
  let join a b =
    match a, b with
    | Bot, _ -> a
    | _, Bot -> b
    | _, Top -> Top
    | Top, _ -> Top
    | NegInf x, NegInf y -> NegInf (max x y)
    | PosInf x, PosInf y -> PosInf (min x y)
    | PosInf _, NegInf _ -> Top
    | NegInf _, PosInf _ -> Top
    
    | Fin (xa, ya), Fin (xb, yb) -> Fin (min xa xb, max ya yb)
    
    | Fin (_, x), NegInf y -> NegInf (max x y) 
    | NegInf y, Fin (_, x) -> NegInf (max x y) 
    
    | Fin (x, _), PosInf y -> PosInf (min x y) 
    | PosInf y, Fin (x, _) -> PosInf (min x y) 
  
  let iszero i =
    match i with
    | Bot -> Bot
    | Top -> Fin (0, 0)
    | NegInf x -> if x >= 0 then Fin (0, 0) else Bot
    | PosInf x -> if x <= 0 then Fin (0, 0) else Bot
    | Fin (x, y) -> if x <= 0 && y >= 0 then Fin (0, 0) else Bot
  let notzero i =
    match i with
    | Fin (0, 0) -> Bot
    | Fin (0, x) -> Fin (1, x)
    | Fin (x, 0) -> Fin (x, -1)
    | NegInf 0   -> NegInf (-1)
    | PosInf 0   -> PosInf 1
    | _ -> i
  let incr i =
    match i with
    | Fin (a, b) -> Fin (a+1, b+1)
    | NegInf x -> NegInf (x+1)
    | PosInf x -> PosInf (x+1)
    | _ -> i
  let decr i =
    match i with
    | Fin (a, b) -> Fin (a-1, b-1)
    | NegInf x -> NegInf (x-1)
    | PosInf x -> PosInf (x-1)
    | _ -> i

  let normalize a =
    match a with
    | Fin (x, y) -> if x > y then Bot else a
    | _ -> a

  let narrow a b =
    match a, b with 
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, _ -> b
    | _, Top -> a
    | NegInf _, NegInf _ -> a
    | NegInf x, PosInf y -> normalize (Fin (y, x))
    | NegInf x, Fin (y, _) -> normalize (Fin (y, x))
  
    | PosInf x, NegInf y -> normalize (Fin (x, y))
    | PosInf _, PosInf _ -> a
    | PosInf x, Fin (_, y) -> normalize (Fin (x, y))
  
    | _ -> a
  
  let widen a b =
    match a, b with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | Top, _ -> Top
    | _, Top -> Top

    | NegInf x, NegInf y     -> if x >= y  then a else Top
    | NegInf x, PosInf y     -> Top
    | NegInf x, Fin (y1, y2) -> if x >= y2 then NegInf x else Top
    
    | PosInf x, NegInf y     -> Top
    | PosInf x, PosInf y     -> if x <= y  then PosInf x else Top
    | PosInf x, Fin (y1, y2) -> if x <= y1 then PosInf x else Top
    
    | Fin (x1, x2), NegInf y -> if x2 >= y then NegInf x2 else Top
    | Fin (x1, x2), PosInf y -> if x1 <= y then PosInf x1 else Top
    | Fin (x1, x2), Fin (y1, y2) ->
       match x1 <= y1, x2 <= y2 with
       |  true,  true -> Fin (x1, x2)
       |  true, false -> PosInf x1
       | false,  true -> NegInf x2
       | false, false -> Top
end

module type AnalysisType =
sig
  type t
  val incr : t -> t
  val decr : t -> t
  val notzero : t -> t
  val iszero  : t -> t

  val containsBot : t * t * t -> bool

  val joinTriple : t * t * t -> t * t * t -> t * t * t

  val triplePP : t * t * t -> string

  val  botTriple : t * t * t
  val initTriple : t * t * t
end

module IntervalState : AnalysisType =
struct
  include Interval
  type t3 = t * t * t

  let botTriple : t3 = (Bot, Bot, Bot)
  let initTriple : t3 = (Top, Fin (0, 0), Fin (0, 0))

  let narrow (a : t3) (b : t3) : t3 =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Interval.narrow a1 b1, Interval.narrow a2 b2, Interval.narrow a3 b3

  let widen (a : t3) (b : t3) : t3 =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Interval.widen a1 b1, Interval.widen a2 b2, Interval.widen a3 b3
  
  type approx = t3 array
                  
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

  let containsBot a =
    match a with
    | Bot, _, _ -> true
    | _, Bot, _ -> true
    | _, _, Bot -> true
    | _ -> false
  
  let joinTriple (a : t3) (b : t3) =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Interval.join a1 b1, Interval.join a2 b2, Interval.join a3 b3
  
  let triplePP a =
    let (x, y, z) = a in
    Printf.sprintf "{x:%12s, y:%12s, z:%12s}"
                   (pp x) (pp y) (pp z)
end

open Cm3

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

  let update (p : program) (ar : (t * t * t) array) : (t * t * t) array =
    let newAr = Array.copy ar in
    let pcounter = ref 0 in
    List.iter (fun i ->
      pcounter := !pcounter + 1;
      let curApp  = ar.(!pcounter - 1) in
      let updList = updateApproxPC i !pcounter curApp in
      List.iter (fun (pc, updApp) ->
        let app = newAr.(pc - 1) in
        if not (containsBot updApp) then
          newAr.(pc - 1) <- joinTriple app updApp
      ) updList
    ) p;
    newAr
  
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
    let oldApp = ref (Array.make (List.length pr) botTriple) in
    let app    = ref (Array.copy !oldApp) in
    !app.(0) <- initTriple;
    while app <> oldApp do
      Printf.printf "%s\n\n" (programApproxPP pr !app);
      let newApp = update pr !app in
      oldApp := !app;
      app    := newApp
    done

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
  let pr1 = fst (List.hd (progP pr_ex4)) in
  let module M = AbstractInterpreter(IntervalState) in
  M.loop pr1
