open! Base

module Err: sig
  type t
  val show: t -> string
end

val translate: ?filename:string -> string -> Err.t list * string option
(** [translate ?filename source]. Converts Flow to TypeScript. *)
