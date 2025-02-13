open Core

let isExample = true

let filename =
  if isExample then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename
let width = String.length (List.hd_exn aoc_input)
let height = List.length aoc_input

type coord = { x : int; y : int }

let idx_of_pos pos = pos.x + (pos.y * width)
let pos_of_idx idx = { x = idx mod width; y = idx / width }

let find_digits line =
  let rec loop acc idx c_list =
    match c_list with
    | [] -> acc
    | c :: tl ->
        if Char.is_digit c then
          loop ((int_of_char c - int_of_char '0', idx) :: acc) (idx + 1) tl
        else loop acc (idx + 1) tl
  in
  loop [] 0 (String.to_list line)

let find_nodes lines =
  List.foldi lines ~init:[] ~f:(fun y acc line ->
      let digits =
        find_digits line
        |> List.map ~f:(fun (dig, dig_idx) -> (dig, dig_idx + (y * width)))
      in
      acc @ digits)

let node_list = find_nodes aoc_input
let result_p1 = 0
let result_p2 = 0
