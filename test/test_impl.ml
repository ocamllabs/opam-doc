(** Comm module

test

test
 *)


type t = int


type v = {a:int; b:int}

module X = 
struct 
  type v = int 
  
    (** val f : v -> v *)
    let f : v -> v = fun a -> a + 1
    (** val f2 : v -> v *)
    let f2 = function {a=i; b=_} as md -> md
end


class point a b = object method f = a+b end

class point_c (point:point) = object method f = point#f end

let f (a : t) : t = a + 1

(** comm avant *)
module M = struct
  let x = 3
  let y = 4
  let f a b = a + b + x + y
  type t = int
  let f2 (a:t) (b:t) = b + a
  module SubM = 
  struct 
    let z = 3 
    module SubSubM = 
    struct 
      let xx = 4 
      type t = v
      let f = function {a=i; b=j} -> i+j
    end 
  end
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
