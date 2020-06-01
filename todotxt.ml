open Angstrom
open Result

let read_file filename =
  let input_line_aux input =
    try Some (input_line input) with End_of_file -> None
  in
  let rec read_lines input =
    match input_line_aux input with
    | Some line ->
        print_endline line;
        read_lines input
    | None -> close_in input
  in
  read_lines (open_in filename)

let is_digit = function
  |'0'..'9' -> true
  | _ -> false

let integer = take_while1 is_digit 
  >>= fun num -> return (int_of_string num)

let dash =
  peek_char
  >>= function
  | Some '-' -> advance 1 >>| fun () -> true
  | _ -> fail "dash expected"

let date =
  integer
  >>= fun year ->
  dash
  >>= fun _ ->
  integer
  >>= fun month ->
  dash
  >>= fun _ ->
  integer
  >>= fun day ->
    return (year, month, day)

let () = match (parse_string ~consume:Consume.All date "2020-5-30") with
| Ok (y,m,d) -> print_int y; print_int m; print_int d; print_newline ()
| Error _ -> print_endline "error"
