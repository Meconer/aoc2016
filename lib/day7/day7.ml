open Core

let isExample = true

let filename =
  if isExample then "lib/day7/example.txt" else "lib/day7/input.txt"

let aoc_input = In_channel.read_lines filename
let resultP1 = 0
let resultP2 = 0
