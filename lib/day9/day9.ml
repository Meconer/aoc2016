open Core

let isExample = true

let filename =
  if isExample then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_lines filename
let resultP1 = 0
let resultP2 = 0
