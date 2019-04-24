open! Base

val first: 'a * 'b -> f:('a -> 'c) -> 'c * 'b
(** [first] from Haskell *)

val second: 'a * 'b -> f:('b -> 'c) -> 'a * 'c
(** [second] from Haskell *)
