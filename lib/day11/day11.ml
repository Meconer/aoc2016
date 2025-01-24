open Core

let isExample = false

let filename =
  if isExample then "lib/day11/example.txt" else "lib/day11/input.txt"

let aoc_input = In_channel.read_lines filename
let resultP1 = 0
let resultP2 = 0
