open! Base

type position = {
  line: int;
  column: int;
} [@@deriving show]

type t = {
  start: position;
  _end: position;
} [@@deriving show]
