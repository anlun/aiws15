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
end

