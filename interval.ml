module Interval =
struct
  type t = Bot | NegInf of int | Fin of int * int | PosInf of int | Top

  type  leftBorder = NInf | LNum of int
  type rightBorder = RNum of int | PInf

  let intervalToBorders i =
    match i with
    | Bot        -> (LNum 1, RNum 0)
    | NegInf x   -> (  NInf, RNum x)
    | Fin (x, y) -> (LNum x, RNum y)
    | PosInf x   -> (LNum x,   PInf)
    | Top        -> (  NInf,   PInf)

  let bordersToInterval b =
    match b with
    | (  NInf, RNum x) -> NegInf x
    | (  NInf,   PInf) -> Top
    | (LNum x,   PInf) -> PosInf x
    | (LNum x, RNum y) -> if x <= y then Fin (x, y) else Bot

  let leftBorderLeq a b =
    match a, b with
    | NInf, _ -> true
    | _, NInf -> false
    | LNum x, LNum y -> x <= y
  
  let rightBorderLeq a b =
    match a, b with
    | _, PInf -> true
    | PInf, _ -> false
    | RNum x, RNum y -> x <= y

  let pp (i : t) =
    match i with
    | Bot -> "/"
    | Top -> "Z"
    | NegInf   x -> Printf.sprintf "(-i,%2i)" x
    | PosInf   x -> Printf.sprintf "(%2i,+i)" x
    | Fin (x, y) -> Printf.sprintf "(%2i,%2i)" x y

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
    | Bot, _ -> b
    | _, Bot -> a
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

  let narrow x y =
    match x, y with
    | Bot, _ -> Bot
    | _, Bot -> Bot
    | _ -> let (a, b) = intervalToBorders x in
           let (c, d) = intervalToBorders y in
           let l = if a = NInf then c else a in
           let r = if b = PInf then d else b in
           bordersToInterval (l, r)

   let widen x y =
    match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | _ -> let (a, b) = intervalToBorders x in
           let (c, d) = intervalToBorders y in
           let l = if  leftBorderLeq a c then a else NInf in
           let r = if rightBorderLeq d b then b else PInf in
           bordersToInterval (l, r)
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
  
  val narrow: t * t * t -> t * t * t -> t * t * t
  val widen : t * t * t -> t * t * t -> t * t * t
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
  
  let containsBot a =
    match a with
    | Bot, _, _ -> true
    | _, Bot, _ -> true
    | _, _, Bot -> true
    | _ -> false
  
  let joinTriple (a : t3) (b : t3) =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    join a1 b1, join a2 b2, join a3 b3
  
  let triplePP a =
    let (x, y, z) = a in
    Printf.sprintf "{x: %7s, y: %7s, z: %7s}"
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
