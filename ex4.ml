open Parity
open Cm3

type approx = t * t * t

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
