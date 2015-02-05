(*
module type AnalysisLattice =
sig
  type t
  val leq  : t -> t -> bool
  val join : t -> t -> t

  val incr : t -> t
  val decr : t -> t
end

module type Bot =
sig
  type t
  val bot : t
end

module type LatticeMeet =
sig
  type t
  val meet : t -> t -> t
end

module type SetOperations =
sig
  type t
  val iszero  : t -> t
  val notzero : t -> t
end
 *)

module BorderValue =
struct
  type t = NegInf | Num of int | PosInf
                                   
  let leq a b =
    match a, b with
    | _, PosInf -> true
    | NegInf, _ -> true
    | Num  x,  Num y -> x <= y
    | _ -> false
  
  let join a b =
    match a, b with
    | PosInf, _ -> PosInf
    | _, PosInf -> PosInf
    | NegInf, _ -> b
    | _, NegInf -> a
    | Num x, Num y -> Num (max x y)
  
  let meet a b =
    match a, b with
    | PosInf, _ -> b
    | _, PosInf -> a
    | NegInf, _ -> NegInf
    | _, NegInf -> NegInf
    | Num x, Num y -> Num (min x y)
  
  let incr a =
    match a with
    | Num x -> Num (x + 1)
    | _ -> a
  
  let decr a =
    match a with
    | Num x -> Num (x - 1)
    | _ -> a
end

module Interval =
struct
  open BorderValue
  type t = BorderValue.t * BorderValue.t

  let bot : t = (PosInf, NegInf)

  (* Bottom normalization *)
  let normalize (i : t) : t =
    match i with
    | PosInf, PosInf -> bot 
    | NegInf, NegInf -> bot 
    | a, b -> if leq a b then i else bot

  let leq a b =
    let (anl, anr), (bnl, bnr) = normalize a, normalize b in 
    

end

