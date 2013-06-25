(** Comm module

test

test
 *)

(** comm avant *)
module M = struct
  let x = 3
  let y = 4
  let f a b = a + b + x + y
  type t = int
  module SubM = struct let z = 3 module SubSubM = struct let xx = 4 end end
end

(** comm type *)
type tabc = M.t

type tbcd = tabc
(** comm type après *)

(** comm type avant *)
type testc = int

module type S = sig
  type persistent_singleton

  val load : unit

  val save : unit

end

module type NP = sig type t end

module Make (Z:NP) : S with type persistent_singleton = Z.t = struct
  type persistent_singleton = Z.t

  let load = ()
  let save = ()
end

module Default_Fonct (A: S) = struct end

module Apply = Default_Fonct(struct type persistent_singleton = int let load = () let save = () end)
