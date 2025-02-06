open Core

let is_example = false

let first_line =
  if is_example then ".^^.^.^^^^"
  else
    "^..^^.^^^..^^.^...^^^^^....^.^..^^^.^.^.^^...^.^.^.^.^^.....^.^^.^.^.^.^.^.^^..^^^^^...^.....^....^."

let no_of_lines_to_generate = if is_example then 10 else 40

let get_next_line line =
  let len = String.length line in
  let get_char i = if i < 0 || i >= len then '.' else String.get line i in
  let next_char i =
    match (get_char (i - 1), get_char i, get_char (i + 1)) with
    | '^', '^', '.' | '.', '^', '^' | '^', '.', '.' | '.', '.', '^' -> '^'
    | _ -> '.'
  in
  String.init len ~f:(fun i -> next_char i)

let rec generate_lines n current_line =
  if n <= 1 then [ current_line ]
  else current_line :: generate_lines (n - 1) (get_next_line current_line)

let no_of_safe_tiles_in_line line =
  String.count line ~f:(fun ch -> Char.equal ch '.')

let lines = generate_lines no_of_lines_to_generate first_line

let result_p1 =
  List.fold lines ~init:0 ~f:(fun acc line ->
      acc + no_of_safe_tiles_in_line line)

let lines = generate_lines 400000 first_line

let result_p2 =
  List.fold lines ~init:0 ~f:(fun acc line ->
      acc + no_of_safe_tiles_in_line line)
