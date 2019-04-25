open! Base

type position = {
  line: int;
  column: int;
} [@@deriving show]

let position_to_string { line; column } =
  Int.to_string line ^ ":" ^ Int.to_string column

type t = {
  start: position;
  _end: position;
} [@@deriving show]

let to_string { start; _end } =
  position_to_string start ^ "-" ^ position_to_string _end

let none = {
  start = { line = 0; column = 0 };
  _end = { line = 0; column = 0 };
}
