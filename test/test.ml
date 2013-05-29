(** Module blabla*)

(** id @return tarace *)
let foo x = x

let l = [1;2;3]

type testz = [ `A | `B | `C ]

type (+'a, 'b) t

type testc = private A of int * float | B of float

type record = {a:int; mutable b:float}

type ('a,'b) test constraint 'a = testc constraint 'b = int

type t2 = int

type testd = int

type testCOMM = 
    A of int * float (** comm pour A of int * float *)
  | B of float * int (** comm pour B of float * int *)

