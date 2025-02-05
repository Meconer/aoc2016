(* open Core *)

let is_example = true

let first_line =
  if is_example then ".^^.^.^^^^"
  else
    "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."

let get_next_line line =
  let len = String.length line in
  let get_char i = if i < 0 || i >= len then '.' else String.get line i in
  let next_char i =
    match (get_char (i - 1), get_char i, get_char (i + 1)) with
    | '^', '^', '.' | '.', '^', '^' | '^', '.', '.' | '.', '.', '^' -> '^'
    | _ -> '.'
  in
  String.init len next_char

let rec generate_lines n current_line =
  if n <= 1 then [ current_line ]
  else current_line :: generate_lines (n - 1) (get_next_line current_line)

let result_p1 = 0
let result_p2 = 0
