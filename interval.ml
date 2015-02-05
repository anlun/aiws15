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

module IntervalState =
struct
  open Interval
  type t = Interval.t * Interval.t * Interval.t

  let narrow (a : t) (b : t) : t =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Interval.narrow a1 b1, Interval.narrow a2 b2, Interval.narrow a3 b3

  let widen (a : t) (b : t) : t =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Interval.widen a1 b1, Interval.widen a2 b2, Interval.widen a3 b3
  
  type approx = t array
                  
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
end
