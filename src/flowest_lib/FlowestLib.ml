open! Base

module Ast = Flow_parser.Flow_ast
module Loc = Flow_parser.Loc
module Parser_flow = Flow_parser.Parser_flow
module Parse_error = Flow_parser.Parse_error
module File_key = Flow_parser.File_key

let show_loc_position (p: Loc.position) =
  Int.to_string p.line ^ ":" ^ Int.to_string p.column

let parse ?(filename: string option) source =
  let filename = Option.map filename ~f:(fun str -> File_key.SourceFile str) in
  Parser_flow.parse_program false filename source

let translate ?(filename: string option) source =
  let (flow_ast, flow_errors) = parse ?filename source in
  List.iter flow_errors ~f:(fun (loc, e) -> Caml.print_endline (* TODO: *)
    @@ show_loc_position loc.start ^ " " ^ Parse_error.PP.error e);
  let ts_ast = Mapper.map_program flow_ast in
  (* Caml.print_endline @@ Ts.Ast.show_program Loc.pp Loc.pp ts_ast; *)
  let ts_str = Ts.Generator.gen_program_simple ts_ast in
  ts_str
