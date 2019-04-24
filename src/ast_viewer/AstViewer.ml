open! Base

module Parser_flow = Flow_parser.Parser_flow
module Ast = Flow_parser.Flow_ast
module Loc = Flow_parser.Loc

let get_ast (source: string) =
  let res = Parser_flow.parse_program false None source in
  let (ast, _) = res in
  ast

let get_ast_string source =
  get_ast source |> Ast.show_program Loc.pp Loc.pp

let read_file filename =
  let buf = Buffer.create 64 in
  let ch =
    if String.(filename = "-")
    then Caml.stdin
    else Caml.open_in filename
  in
  try
    while true; do
      Buffer.add_char buf (Caml.input_char ch)
    done;
    failwith "infinite loop"
  with End_of_file ->
    Caml.close_in ch;
    Buffer.contents buf

let exit_with_error (msg: string) =
  Caml.Printf.eprintf "%s" msg;
  Caml.exit 1

let () =
  if Array.length Sys.argv < 2 then
    exit_with_error "Empty filename";
  let filename = Sys.argv.(1) in
  let content = read_file filename in
  let ast = get_ast_string content in
  Caml.print_endline "(*";
  Caml.print_endline content;
  Caml.print_endline "*)";
  Caml.print_newline ();
  Caml.print_endline "let _ =";
  Caml.print_endline ast
