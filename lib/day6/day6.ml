open Core

let isExample = true

let filename =
  if isExample then "lib/day6/example.txt" else "lib/day6/input.txt"

let aoc_input = In_channel.read_lines filename


let parse_input lines =

  let length = String.length (List.hd_exn lines) in
  let start_lists = List.init length ~f:(fun _ -> []) in

  let rec loop acc lines =
  match lines with 
  | [] -> List.rev acc
  | line:: rest ->
    let lst = String.to_list line in
    let new_lists = List.foldi lst ~init:acc ~f:(fun idx ch ->
       )




let resultP1 = 0
let resultP2 = 0
