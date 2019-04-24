open! Base
open! FlowestLib

let read_file filename =
    let ch = Caml.open_in filename in
    let str = Caml.really_input_string ch (Caml.in_channel_length ch) in
    Caml.close_in ch;
    str

let write_file filename str =
  let ch = Caml.open_out filename in
  Caml.output_string ch str;
  Caml.close_out ch

let () =
  if Array.length Sys.argv < 3 then begin
    Caml.print_endline "Empty filename";
    Caml.exit 1;
  end;
  let filename = Sys.argv.(1) in
  let output_filename = Sys.argv.(2) in
  let input_string = read_file filename in
  let output = translate ~filename input_string in
  write_file output_filename output
