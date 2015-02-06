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

  let leq x y =
    match x, y with
    | Bot, _ -> true
    | _, Bot -> false
    | _ -> let (a, b) = intervalToBorders x in
           let (c, d) = intervalToBorders y in
           let isLBigger =  leftBorderLeq c a in
           let isRBigger = rightBorderLeq b d in
           isLBigger && isRBigger
    
 let join x y =
    match x, y with
    | Bot, _ -> y
    | _, Bot -> x
    | _ -> let (a, b) = intervalToBorders x in
           let (c, d) = intervalToBorders y in
           let l = if  leftBorderLeq a c then a else c in
           let r = if rightBorderLeq b d then d else b in
           bordersToInterval (l, r)

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

open AbstractInterpreter

module IntervalState : AnalysisType =
struct
  include Interval
  type t3 = t * t * t

  let  botTriple : t3 = (Bot, Bot, Bot)
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
