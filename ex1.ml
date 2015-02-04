module type CompleteLattice =
  sig
    type t
    val bot : t
    val top : t

    val join : t -> t -> t
    val meet : t -> t -> t
  end

module KleeneIteration (M : CompleteLattice) =
  struct 
    let lfp (f : M.t -> M.t) : M.t =
      let rec lfp_acc f v =
	let nv = f v in
	if nv == v then v else lfp_acc f nv
      in
      lfp_acc f M.bot
  end

(* -- *)

module type TransitionSystem =
  sig
    type t
    val transition : t -> t list
    val initStates : t list
  end

module TS1 : TransitionSystem =
  struct
    type t = int
    let transition (s : t) : t list = []
    let initStates : t list = [0]
  end

module TS2 : TransitionSystem =
  struct
    type t = L | R
    let transition (s : t) : t list =
      match s with
      | L -> [R]
      | R -> [L]
    let initStates : t list = [L]
  end

module TS3 : TransitionSystem =
  struct
    type t = int
    let transition (s : t) : t list = [s + 1]
    let initStates : t list = [0]
  end

module ReachStates (T : TransitionSystem) =
  struct
    let f (states : T.t list) : T.t list =
      List.concat (List.map T.transition states)
    let collectSemantics : T.t list =
      let rec lfp_acc f v =
	let nv = f v in
	if nv == v then v else lfp_acc f v
      in
      lfp_acc f T.initStates
  end
