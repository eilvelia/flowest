open! Base

module FlowLoc = Flow_parser.Loc
module Parser_flow = Flow_parser.Parser_flow
module Parse_error = Flow_parser.Parse_error
module File_key = Flow_parser.File_key

let show_flow_position (p: FlowLoc.position) =
  Int.to_string p.line ^ ":" ^ Int.to_string p.column

let show_flow_loc (l: FlowLoc.t) =
  show_flow_position l.start ^ "-" ^ show_flow_position l._end

let parse_flow ?(filename: string option) source =
  let filename = Option.map filename ~f:(fun str -> File_key.SourceFile str) in
  Parser_flow.parse_program false filename source

module Err = struct
  type t =
    | FlowParseError of string
    | MapperError of string

  let show: t -> _ = function
    | FlowParseError s -> s
    | MapperError s -> s

  let of_flow_error ((loc, err): (FlowLoc.t * Parse_error.t)) =
    let s = show_flow_loc loc ^ " " ^ Parse_error.PP.error err in
    FlowParseError s

  let of_mapper_error (e: Ts.Loc.t * string) =
    let s = Mapper.show_error e in
    MapperError s
end

module RawTs = struct
  let prefix = "$$"

  let prefix_len = String.length prefix

  let convert_ast ((loc, stats, comms): (Ts.Loc.t, Ts.Loc.t) Ts.Ast.program) =
    let open Ts.Ast in
    let raw loc str = loc, Statement.RawTs (String.strip str) in
    let f (l, t' as c) (stats', comms') = match t' with
      | Comment.Block s when String.is_prefix s ~prefix ->
        raw l (String.drop_prefix s prefix_len) :: stats', comms'
      | _ -> stats', c :: comms'
    in
    let (raw_list, comms') = List.fold_right comms ~f ~init:([], []) in
    loc, stats @ raw_list, comms'
end

let translate ?(filename: string option) source : Err.t list * string option =
  let source = Preprocessor.replace source in
  let (flow_ast, flow_errors) = parse_flow ?filename source in
  let errors = List.map flow_errors ~f:Err.of_flow_error in
  try
    let ts_ast = Mapper.map_program flow_ast in
    let ts_ast = Preprocessor.filter_directive_comments ts_ast in
    let ts_ast = RawTs.convert_ast ts_ast in
    (* Caml.print_endline @@ Ts.Ast.show_program Loc.pp Loc.pp ts_ast; *)
    let ts_str = Ts.Generator.gen_program ts_ast in
    let ts_str = ts_str ^ "\n" in
    (errors, Some ts_str)
  with Mapper.Error (l, s) ->
    (errors @ [Err.of_mapper_error (l, s)], None)
