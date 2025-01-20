open Core

let isExample = true

let filename =
  if isExample then "lib/day7/example.txt" else "lib/day7/input.txt"

let aoc_input = In_channel.read_lines filename

let supports_TLS line =
  let rec loop in_bracket out_bracket line =
  let fb_pos = String.index line '[' in
  let sb_pos = String.index line ']' in
  match fb_pos with 

(* let solve_p1 lines =
   List.count lines ~f:(fun line -> supports_TLS line) *)
let resultP1 = 0
let resultP2 = 0
