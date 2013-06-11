(** MLI doc - module info *)

(** test fun fun sig *)
class dummy: int -> float -> object val x : int end

(** test fun fun constr *)
class bla2 : int -> int -> dummy

class ['a, 'b, 'c] dummy_params: object end

class point : int -> int -> 
object 
end

(** comm type t 
    @author myself
*)
type (+'a, 'b) t

(** comm testz *)
type testz = [ `A | `B | `C ]

(** Foo identity 
    @author ookok
*)
val foo : 'a -> 'a

(** Descr list l 
    @see 'test.ml' seeinfo    
    @see "ita" seeinfo2
    @see "ita" seeinfo3
*)
val l : int list

(** comm testComm *)
type testCOMM = private
  | One of int * float (** comm for A of int * float *)
  | Two of float * int (** comm for B of float * int *)

(** comm testc *)
type testc = A of int * float | B of float

(** comm testd *)
type testd = int

(** comm t2 *)
type t2 = int

(** A record type's comm *)
type record = {a:int; (** comm for field a *)
	       mutable b:float}
(*
(** comm type ('a,'b) test constraint 'a = testc constraint 'b = int *)
type ('a,'b) test constraint 'a = testc constraint 'b = int
*)
(*
(** lblgtk type *)
type tTestLBLGTK = Gpointer.optstring
*)
