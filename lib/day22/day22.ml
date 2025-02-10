open Core

let isExample = false

let filename =
  if isExample then "lib/day22/example.txt" else "lib/day22/input.txt"

let aoc_input = In_channel.read_lines filename
let aoc_input = List.sub aoc_input ~pos:2 ~len:(List.length aoc_input - 2)

type node_t = { x : int; y : int; size : int; used : int }

let get_node line =
  let x, y, size, used =
    Scanf.sscanf line "/dev/grid/node-x%d-y%d     %dT   %dT    %dT   %d"
      (fun x y s u _ _ -> (x, y, s, u))
  in
  { x; y; size; used }

let result_p1 = 0
let result_p2 = 0
