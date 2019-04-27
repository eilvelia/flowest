open! FlowestLib

(* TODO: Use lwt *)
(* TODO: Perhaps use dune for diffing *)

let (/) = Filename.concat

let dirname = ".." / ".." / ".." / "test" / "expect"

let get_file_list () =
  Sys.readdir dirname
    |> Array.to_list
    |> List.filter (fun x -> not (Sys.is_directory (dirname / x)))

let js_ext = ".js"
let is_js_file x = Filename.check_suffix x js_ext
let chop_js_suffix x = Filename.chop_suffix x js_ext

let to_js_file x = x ^ ".js"
let to_exp_file x = x ^ ".exp.d.ts"
let to_out_file x = x ^ ".output"

let read_file filename =
  let ch = open_in filename in
  let str = really_input_string ch (in_channel_length ch) in
  close_in ch;
  str

let write_file filename str =
  let ch = open_out filename in
  output_string ch str;
  close_out ch

let show_diff file1 file2 =
  flush_all ();
  let pid = Unix.create_process "diff" [|"diff"; (* "-u"; *) file1; file2|]
    Unix.stdin Unix.stdout Unix.stderr in
  Unix.waitpid [Unix.WUNTRACED] pid |> ignore

let print_errors errors =
  List.iter (fun e -> prerr_endline @@ Translator.Err.show e) errors

let check test_name : bool =
  Caml.print_newline ();
  print_endline @@ "Test: " ^ test_name;
  let js_file = dirname / to_js_file test_name in
  let exp_file_r = to_exp_file test_name in
  let exp_file = dirname / exp_file_r in
  let js = read_file js_file in
  let (errors, str_opt) = Translator.translate ~filename:test_name js in
  print_errors errors;
  match str_opt with
    | None -> prerr_endline "Translation failed"; false
    | Some str when Sys.file_exists exp_file ->
      let expected = read_file exp_file in
      let same = str = expected in
      if not same then begin
        let out_file_r = to_out_file test_name in
        let out_file = dirname / out_file_r in
        Printf.printf "Files %s and %s differ\n" exp_file_r out_file_r;
        write_file out_file str;
        show_diff exp_file out_file;
      end else begin
        print_endline "Ok";
      end;
      same
    | Some _ -> prerr_endline "No expectation file"; true

let promote test_name : bool =
  print_endline @@ "Promote: " ^ test_name;
  let js = read_file (dirname / to_js_file test_name) in
  let exp_file = dirname / to_exp_file test_name in
  let (errors, str_opt) = Translator.translate ~filename:test_name js in
  print_errors errors;
  match str_opt with
    | None -> prerr_endline "Translation failed"; false
    | Some result -> write_file exp_file result; true

let get_test_names () =
  let files = get_file_list () in
  let js_files = List.filter is_js_file files in
  let test_names = List.map chop_js_suffix js_files in
  test_names

let decode_list str =
  String.split_on_char ',' str |> List.map String.trim

let bool_to_int = function false -> 0 | true -> 1

let check_many cases =
  let bools = List.map check cases in
  let success = List.for_all ((=) true) bools in
  if not success then begin print_endline "exit 1"; exit 1 end;
  ()

let promote_many cases =
  let n = List.fold_left (fun a b -> a + bool_to_int (promote b)) 0 cases in
  Printf.printf "Promoted %d files" n

let check_specified str = check_many (decode_list str)
let check_all () = check_many (get_test_names ())

let promote_specified str = promote_many (decode_list str)
let promote_all () = promote_many (get_test_names ())

let non_empty_str s = String.length s > 0

let main =
  let promote_str = ref "" in
  let promote_all_enabled = ref false in
  let check_str = ref "" in
  let speclist = Arg.[
    "--promote", Set_string promote_str, "Examples: 'function', 'function,exact'";
    "--promote-all", Set promote_all_enabled, "Promote all";
    "--check", Set_string check_str, "Check specified tests";
  ] in
  Arg.parse speclist ignore "Usage: $ TestRunner [options]";
  match () with
    | _ when !promote_all_enabled -> promote_all ()
    | _ when non_empty_str !promote_str -> promote_specified !promote_str
    | _ when non_empty_str !check_str -> check_specified !check_str
    | _ -> check_all ()
