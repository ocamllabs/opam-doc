(** module test 2 *)

(** type t3 comm *)
type t3 = Bla of Test_impl.testc | Bli of Pervasives.in_channel

(** blabla *)
let f n = Bli (Pervasives.stdin)

type t = Test_impl.M.t

module A = struct

  module A' = struct let x = 1 end
    
  module A'' = struct let y = 1 end

  let z = 1
end
