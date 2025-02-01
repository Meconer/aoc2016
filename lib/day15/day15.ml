open Core

let isExample = false

let filename =
  if isExample then "lib/day15/example.txt" else "lib/day15/input.txt"

let aoc_input = In_channel.read_lines filename

type disc_t = { idx : int; no_of_pos : int; start_pos : int }

let parse_line line =
  Scanf.sscanf line
    "Disc #%d has %d positions; at time=0, it is at position %d."
    (fun idx no_of_pos start_pos -> { idx; no_of_pos; start_pos })

let parse_input lines =
  let rec loop acc lines =
    match lines with
    | [] -> List.rev acc
    | line :: rest -> loop (parse_line line :: acc) rest
  in
  loop [] lines

let pos_for_disc_at_time disc time =
  (time + disc.idx + disc.start_pos) mod disc.no_of_pos

let find_common disc_1 disc_2 start diff =
  (* pos = (time + idx + start_pos) mod n_pos
    0 = (t + i + s) mod np => t =  n * (np - s - i)
    For the first example disc n * (5 - 4 - 1) gives 0 if we start at time 0, 5, 10, 15, 20 etc
    For the second disc n * (2 - 1 - 2) gives 0 at 1, 3, 5, 7 etc. First common pos at t=5. 
    Next time is at 15. Diff is 5 * 2 = 10 
  *)
  let rec loop time =
    let pos_1 = pos_for_disc_at_time disc_1 time in
    let pos_2 = pos_for_disc_at_time disc_2 time in
    if pos_1 = 0 && pos_2 = 0 then time else loop (time + diff)
  in
  let first = loop start in
  let second = loop (first + 1) in
  (first, second)

let result_p1 = 0
let result_p2 = 0
