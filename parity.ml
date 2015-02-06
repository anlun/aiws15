module Parity =
struct 
  type t = Bot | Even | Odd | Top
  
  let bot = Bot

  let leq x y =
    match x, y with
    |  Bot, _    -> true
    | Even, Even -> true
    |  Odd, Odd  -> true
    |    _, Top  -> true
    |    _       -> false

  let join x y =
    match x, y with
    |  Top,    _ -> Top
    |    _,  Top -> Top
    | Even, Even -> Even
    |  Odd,  Odd -> Odd
    |  Bot,    _ -> y 
    |    _,  Bot -> x 
    | _ -> Top

  let iszero x =
    match x with
    | Bot  -> Bot
    | Even -> Even
    | Odd  -> Bot
    | Top  -> Even
  
  let notzero x =
    match x with
    | Bot  -> Bot
    | Even -> Even
    | Odd  -> Odd
    | Top  -> Top
  
  let incr x =
    match x with
    | Bot  -> Bot
    | Even -> Odd
    | Odd  -> Even 
    | Top  -> Top
  
  let decr x =
    match x with
    | Bot  -> Bot
    | Even -> Odd
    | Odd  -> Even 
    | Top  -> Top

  let pp p =
    match p with
    | Bot  -> "/"
    | Even -> "E"
    | Odd  -> "O"
    | Top  -> "Z"

  let widen  = join (* TODO *)
  let narrow = join (* TODO *)
end

open AbstractInterpreter

module ParityState : AnalysisType =
struct
  include Parity
    type t3 = t * t * t

  let  botTriple : t3 = (Bot,  Bot,  Bot)
  let initTriple : t3 = (Top, Even, Even)

  let narrow (a : t3) (b : t3) : t3 =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Parity.narrow a1 b1, Parity.narrow a2 b2, Parity.narrow a3 b3

  let widen (a : t3) (b : t3) : t3 =
    let (a1, a2, a3) = a in
    let (b1, b2, b3) = b in
    Parity.widen a1 b1, Parity.widen a2 b2, Parity.widen a3 b3
  
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
    Printf.sprintf "{x: %3s, y: %3s, z: %3s}"
                   (pp x) (pp y) (pp z)

end
