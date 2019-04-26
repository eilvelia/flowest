open! Base

let special_prefix = "$$"

let special_prefix_len = String.length special_prefix

let raw_ts_comments ((loc, stats, comms): (Ts.Loc.t, Ts.Loc.t) Ts.Ast.program) =
  let open Ts.Ast in
  let raw loc str = loc, Statement.RawTs (String.strip str) in
  let f (l, t' as c) (stats', comms') = match t' with
    | Comment.Block s when String.is_prefix s ~prefix:special_prefix ->
      raw l (String.drop_prefix s special_prefix_len) :: stats', comms'
    | _ -> stats', c :: comms'
  in
  let (raw_list, comms') = List.fold_right comms ~f ~init:([], []) in
  loc, stats @ raw_list, comms'
