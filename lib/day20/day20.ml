open Core

let isExample = false

let filename =
  if isExample then "lib/day20/example.txt" else "lib/day20/input.txt"

let max_valid = if isExample then 9 else 4294967295
let aoc_input = In_channel.read_lines filename

let get_interval_of_line line =
  let parts = String.split line ~on:'-' |> List.map ~f:int_of_string in
  (List.hd_exn parts, List.last_exn parts)

let merge_intervals sorted =
  let rec loop acc sorted =
    match sorted with
    | [] -> acc
    | a :: [] -> a :: acc
    | a :: b :: rest ->
        if snd a < fst b - 1 then loop (a :: acc) (b :: rest)
        else loop acc ((fst a, max (snd a) (snd b)) :: rest)
  in
  loop [] sorted

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
  let invalids = merge_intervals sorted in
  let sorted =
    List.sort invalids ~compare:(fun a b -> Int.compare (fst a) (fst b))
  in
  snd (List.hd_exn sorted) + 1

let result_p1 = solve_p1 aoc_input

let count_free sorted max_valid =
  let rec loop acc sorted =
    match sorted with
    | [] -> acc
    | a :: [] -> acc + max_valid - snd a
    | a :: b :: rest ->
        let n_acc = acc + fst b - snd a - 1 in
        loop n_acc (b :: rest)
  in
  loop 0 sorted

let solve_p2 lines =
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
  let invalids = merge_intervals sorted in
  let sorted =
    List.sort invalids ~compare:(fun a b -> Int.compare (fst a) (fst b))
  in
  count_free sorted max_valid

let result_p2 = solve_p2 aoc_input
