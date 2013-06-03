(** MLI doc *)

(** type t kikoo kikoo
    @author moi
*)
type (+'a, 'b) t

(** comm testz *)
type testz = [ `A | `B | `C ]

(** Foo identity 
    @author ookok
*)
val foo : 'a -> 'a

(** Descr liste l 
    @see 'test.ml' seeinfo
    @see <infsup> seeinfo2
    @see "guill" seeinfo3
*)
val l : int list

(** comm testComm *)
type testCOMM = 
  | A of int * float (** comm pour A of int * float *)
  | B of float * int (** comm pour B of float * int *)

(** comm testc *)
type testc = private A of int * float | B of float

(** comm testd *)
type testd = int

(** comm t2 *)
type t2 = int

(** comm record field *)
type record = {a:int; (** comm pour record field a *)

	       mutable b:float}

(** comm type ('a,'b) test constraint 'a = testc constraint 'b = int *)
type ('a,'b) test constraint 'a = testc constraint 'b = int
