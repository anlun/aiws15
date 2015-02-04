open Parity
open

type approx = t * t * t

let approxPP a = let (x, y, z) = a in
                 Printf.sprintf "{x:%3s, y:%3s, z:%3s}"
                                (parityPP x) (parityPP y) (parityPP z)

let updateApprox (i : inst) a
