open! Base

module Directive = struct
  type t =
    | RemoveNextLine
    | RemoveLine

  let equal t1 t2 =
    match t1, t2 with
    | RemoveNextLine, RemoveNextLine -> true
    | RemoveLine, RemoveLine -> true
    | _ -> false

  let equal_opt = equal_option equal

  let (=) = equal
  let (=?) = equal_opt

  let prefix = "flowest-"

  let prefix_len = String.length prefix

  let parse_modifier = function
    | "remove-next-line" | "rnl" -> Some RemoveNextLine
    | "remove-line" | "rl" -> Some RemoveLine
    | _ -> None

  let parse (s: string): t option =
    let open Option in
    String.substr_index s ~pattern:prefix >>= fun i ->
      let i = i + prefix_len in
      let end_ = String.index_from s i ' ' in
      let end_ = Option.value end_ ~default:(String.length s) in
      let directive_str = String.sub s ~pos:i ~len:(end_ - i) in
      parse_modifier directive_str

  let parse_pure (s: string): t option =
    let s = String.strip s in
    let is_directive = String.is_prefix s ~prefix in
    if is_directive
    then parse_modifier @@ String.drop_prefix s prefix_len
    else None
end

let replace (s: string) =
  let lines = String.split s ~on:'\n' in
  let rec go acc = function
    | x :: y :: xs ->
      begin match Directive.parse x with
      | Some RemoveNextLine -> go ("" :: x :: acc) xs
      | Some RemoveLine -> go ("" :: acc) (y :: xs)
      | None -> go (x :: acc) (y :: xs)
      end
    | x :: [] when Directive.(parse x =? Some RemoveLine) -> acc
    | x :: [] -> x :: acc
    | [] -> acc
  in
  let res_lines = List.rev @@ go [] lines in
  String.concat ~sep:"\n" res_lines

let is_directive_comment: 'a. 'a Ts.Ast.Comment.t -> bool =
  let open Ts.Ast.Comment in
  fun (_, (Block s | Line s)) ->
    Directive.parse_pure s |> Option.is_none

let filter_directive_comments (p: (_, _) Ts.Ast.program): (_, _) Ts.Ast.program =
  let (loc, stats, comms) = p in
  (loc, stats, List.filter comms ~f:is_directive_comment)
