(** Module descr *)

(** on open test *)
open Test

(** type t3 comm
@see 'this' bla
*)
type t3 = A of testc | B of Pervasives.in_channel

(** dummy f function *)
val f : int -> Pervasives.in_channel
