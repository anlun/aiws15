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

(* -- *)

let parityPP p =
  match p with
  | Bot  -> "bot"
  | Even -> "evn"
  | Odd  -> "odd"
  | Top  -> "top"
