(**Comm module 1*)

(** id @author qqchose *)
let foo x = x

class dummy = object end

class dummy2 = object val x = 1 method b a b () = a + b end

class point a b = object 
  val x = a 
  val y = b 
  method getX = x
  method getY = y
  method add_coord () = x + y
end

class point2 = point

let l = [1;2;3]

(** Test include Array *)
include Array

external beurk : int -> int = "Beurk"

(** {6 Module test } *)

(** test exc 1 *)
exception Blabla of int * float

(** test exc 2 *)
exception Blibli of int * (float * int)

type texc = int

(** test exc 3 *)
exception Bloublou of texc

(** Comm module sig *)
module M = struct
  let x = 3
  let z = 4
  type t = int
end

(** Comm module ident *)
module Mmod = Int32

module type Mtyp =
  sig
    type t
    type u
    type v
    type w    
  end

(** Comm module functor *)
module Mfunct = functor (Mt : Mtyp) ->
  struct
  end

(** Comm module functor++ *)
module MfunctMul (Mt : Mtyp) (Mt2 : Mtyp) (Mt3 : Mtyp) = 
  struct
  end

(** Test Module rec Event *)
module rec Even : sig
  type t = Zero | Succ of Odd.t
end = struct
  type t = Zero | Succ of Odd.t
end
and 
(** Test Module rec Odd *)
Odd : sig
  type t = Succ of Even.t
end = struct
  type t = Succ of Even.t
end

(** Test apply *)
module TestMap = Map.Make(String)

(** Test apply 2 *)
module TestApply2 = Map.Make(struct type t = int let compare x y = 1 end)


module type MconstrFunctDummy =
  sig
    type a
  end

(** Test constraint *)
module type Mconstr = Mtyp with type t = int and type u = float and type v = char

module Mdum = struct let x = 13 end

(** Test constraint 2 *)
module type S = sig type t module M' : sig end end with module M' = Mdum and type t = int

module type D = sig end

(** Test packaged module *)
let dpackaged : (string, (module D)) Hashtbl.t = Hashtbl.create 17

(** blibli *)
type testz = [ `A | `B | `C ]

type (+'a, 'b) t

(** Test mutual naming *)
let t (a : (int Queue.t, Pervasives.in_channel) t) = ()

(** deuxieme *)
let t i = i + 1

(** comm testc *)
type testc = A of int * float | B of float

(** {1 IMPORTANT} *)

(** One record type*)
type record = {a:int; mutable b:float}

type ('a,'b) test constraint 'a = testc constraint 'b = int

(** comm typ t2 *)
type t2 = int

type testd = int

(** comm test comm *)
type testCOMM = 
    A of int * float (** comm pour A of int * float *)
  | B of float * int (** comm pour B of float * int *)

