open Core

let isExample = false

let filename =
  if isExample then "lib/day2/example.txt" else "lib/day2/input.txt"

let aoc_input = In_channel.read_lines filename

type dirT = Up | Down | Left | Right
type posT = { x : int; y : int }

let grid = [| 1; 2; 3; 4; 5; 6; 7; 8; 9 |]

let grid_p2 =
  [|
    ' ';
    ' ';
    '1';
    ' ';
    ' ';
    ' ';
    '2';
    '3';
    '4';
    ' ';
    '5';
    '6';
    '7';
    '8';
    '9';
    ' ';
    'A';
    'B';
    'C';
    ' ';
    ' ';
    ' ';
    'D';
    ' ';
    ' ';
  |]

let width = 3
let width_p2 = 5
let height = 3
let height_p2 = 5
let idx_of_pos pos = pos.x + (pos.y * width)
let idx_of_pos_p2 pos = pos.x + (pos.y * width_p2)
let pos_of_idx idx = { x = idx mod width; y = idx / width }
let pos_of_idx_p2 idx = { x = idx mod width_p2; y = idx / width_p2 }

let dir_of_char c =
  match c with
  | 'U' -> Up
  | 'L' -> Left
  | 'D' -> Down
  | 'R' -> Right
  | _ -> failwith "wrong dir char"

let do_move start dir =
  match dir with
  | Up -> { start with y = max 0 (start.y - 1) }
  | Down -> { start with y = min (height - 1) (start.y + 1) }
  | Right -> { start with x = min (width - 1) (start.x + 1) }
  | Left -> { start with x = max (start.x - 1) 0 }

let do_move_p2 start dir =
  let new_pos =
    match dir with
    | Up -> { start with y = max 0 (start.y - 1) }
    | Down -> { start with y = min (height_p2 - 1) (start.y + 1) }
    | Right -> { start with x = min (width_p2 - 1) (start.x + 1) }
    | Left -> { start with x = max (start.x - 1) 0 }
  in
  let char_at_pos = grid_p2.(idx_of_pos_p2 new_pos) in
  if Char.equal char_at_pos ' ' then start else new_pos

let do_moves line start =
  String.fold ~init:start line ~f:(fun pos ch ->
      let dir = dir_of_char ch in
      do_move pos dir)

let do_moves_p2 line start =
  String.fold ~init:start line ~f:(fun pos ch ->
      let dir = dir_of_char ch in
      let np = do_move_p2 pos dir in
      np)

let solve_p1 lines =
  let rec loop acc start lines =
    match lines with
    | [] -> acc
    | line :: rest ->
        let pos = do_moves line start in
        let key = grid.(idx_of_pos pos) in
        loop ((acc * 10) + key) pos rest
  in

  loop 0 { x = 1; y = 1 } lines

let solve_p2 lines =
  let rec loop acc start lines =
    match lines with
    | [] -> String.of_char_list (List.rev acc)
    | line :: rest ->
        let pos = do_moves_p2 line start in
        let key = grid_p2.(idx_of_pos_p2 pos) in

        loop (key :: acc) pos rest
  in

  loop [] { x = 0; y = 2 } lines

let resultP1 = solve_p1 aoc_input
let resultP2 = solve_p2 aoc_input
