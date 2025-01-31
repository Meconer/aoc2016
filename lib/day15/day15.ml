open Core

let isExample = false

let filename =
  if isExample then "lib/day15/example.txt" else "lib/day15/input.txt"

let aoc_input = In_channel.read_lines filename
let result_p1 = 0
let result_p2 = 0
