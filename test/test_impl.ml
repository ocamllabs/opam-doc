(**Comm module 1*)

(** id @author qqchose *)
let foo x = x

let l = [1;2;3]

(** blibli *)
type testz = [ `A | `B | `C ]

type (+'a, 'b) t

(** comm testc *)
type testc = A of int * float | B of float

type record = {a:int; mutable b:float}

type ('a,'b) test constraint 'a = testc constraint 'b = int

(** comm typ t2 *)
type t2 = int

type testd = int

(** comm test comm *)
type testCOMM = 
    A of int * float (** comm pour A of int * float *)
  | B of float * int (** comm pour B of float * int *)

