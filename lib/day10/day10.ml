open Core

let isExample = false

let filename =
  if isExample then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_lines filename

let solve_p1 lines = 
  let rec loop bots lines =
    match lines with 
    | []->bots
    | line::rest ->
      if String.is_prefix "value" then
        let bot_no, value = Scanf

let resultP1 = 0
let resultP2 = 0
