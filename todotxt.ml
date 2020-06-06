open Angstrom
open Result

let read_file filename =
  let input_line_aux input = try Some (input_line input) with End_of_file -> None in
  let rec read_lines input =
    match input_line_aux input with
    | Some line ->
        print_endline line;
        read_lines input
    | None -> close_in input
  in
  read_lines (open_in filename)

let is_space = function ' ' | '\t' -> true | _ -> false

let is_digit = function '0' .. '9' -> true | _ -> false

let spaces = skip_while is_space

let integer = take_while1 is_digit >>= fun num -> return (int_of_string num)

let dash = char '-'

let left_brace = char '('

let right_brace = char ')'

let date =
  let validate_date month day =
    match month with
    | 2 -> day > 0 && day < 30
    | 4 | 6 | 9 | 11 -> day > 0 && day < 31
    | 1 | 3 | 5 | 7 | 8 | 10 | 12 -> day > 0 && day < 32
    | _ -> false
  in
  integer >>= fun year ->
  dash >>= fun _ ->
  integer >>= fun month ->
  dash >>= fun _ ->
  integer >>= fun day -> if validate_date month day then return (year, month, day) else fail "invalid date"

let priority = peek_char

let parse_todo txt = parse_string ~consume:Consume.All date txt

let () =
  match parse_todo "2020-5-30" with
  | Ok (y, m, d) ->
      print_int y;
      print_int m;
      print_int d;
      print_newline ()
  | Error _ -> print_endline "error"
