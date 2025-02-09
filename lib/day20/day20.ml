open Core

let isExample = false

let filename =
  if isExample then "lib/day20/example.txt" else "lib/day20/input.txt"

let max_valid = if isExample then 9 else 4294967295
let aoc_input = In_channel.read_lines filename

let get_interval_of_line line =
  let parts = String.split line ~on:'-' |> List.map ~f:int_of_string in
  (List.hd_exn parts, List.last_exn parts)

let in_interval range no =
  no >= fst range && no <= snd range

let calc_resulting_invalids sorted =
  let rec loop acc sorted =
    match sorted with 
    | [] -> acc
    | interval :: rest -> 
      let start, stop = interval in
      if List.is_empty acc then loop [(start, stop)] rest
      else
        let n_acc = List.fold ~init:acc ~f:(fun acc el ->
            if in_interval el start then 
          )


let solve_p1 lines =
  let rec get_intervals intervals lines =
    match lines with
    | [] -> intervals
    | hd :: tl ->
        let start, stop = get_interval_of_line hd in
        get_intervals ((start, stop) :: intervals) tl
  in
  let invalids = get_intervals [] lines in
  let sorted =
    List.sort invalids ~compare:(fun a b -> Int.compare (fst a) (fst b))
  in
  let invalids = calc_resulting_invalids sorted in
  invalids

let result_p1 = 0
let result_p2 = 0
