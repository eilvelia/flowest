open! Base

type error

val show_error: error -> string

val translate: ?filename:string -> string -> error list * string option
(** [translate ?filename source]. Converts Flow to TypeScript. *)
