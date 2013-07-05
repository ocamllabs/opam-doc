include Test_include

let f x = x + 1

(** bla bli blou
    bleble
*)
module type S = sig type t end


module F (A:S) = struct type t = A.t end

module F' (A: sig type t end) = struct type t = A.t end

module F2 (A:S) = F (A)

module F3 = F
