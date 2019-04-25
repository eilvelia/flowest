open! Base

module Ast = Flow_parser.Flow_ast
module Loc = Flow_parser.Loc
module Parser_flow = Flow_parser.Parser_flow
module Parse_error = Flow_parser.Parse_error
module File_key = Flow_parser.File_key

let show_flow_position (p: Loc.position) =
  Int.to_string p.line ^ ":" ^ Int.to_string p.column

let show_flow_loc (l: Loc.t) =
  show_flow_position l.start ^ "-" ^ show_flow_position l._end

let parse ?(filename: string option) source =
  let filename = Option.map filename ~f:(fun str -> File_key.SourceFile str) in
  Parser_flow.parse_program false filename source

type error =
  | FlowParseError of string
  | MapperError of string
let show_error: error -> _ = function
  | FlowParseError s -> s
  | MapperError s -> s
let of_flow_error ((loc, err): (Loc.t * Parse_error.t)) =
  let s = show_flow_loc loc ^ " " ^ Parse_error.PP.error err in
  FlowParseError s
let of_mapper_error (e: Ts.Loc.t * string) =
  let s = Mapper.show_error e in
  MapperError s

let translate ?(filename: string option) source : error list * string option =
  let source = Preprocessor.replace source in
  let (flow_ast, flow_errors) = parse ?filename source in
  let errors = List.map flow_errors ~f:of_flow_error in
  try
    let ts_ast = Mapper.map_program flow_ast in
    let ts_ast = Preprocessor.filter_ast_comments ts_ast in
    (* Caml.print_endline @@ Ts.Ast.show_program Loc.pp Loc.pp ts_ast; *)
    let ts_str = Ts.Generator.gen_program_simple ts_ast in
    (errors, Some ts_str)
  with Mapper.Error (l, s) ->
    (errors @ [of_mapper_error (l, s)], None)
