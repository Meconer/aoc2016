open Core

let isExample = false

let filename =
  if isExample then "lib/day6/example.txt" else "lib/day6/input.txt"

let aoc_input = In_channel.read_lines filename

let get_most_used_char ch_list =
  let list = List.sort ch_list ~compare:Char.compare in
  let rec loop acc last_char count list =
    match list with
    | [] ->
        let sorted =
          List.sort acc ~compare:(fun a b -> Int.descending (snd a) (snd b))
        in
        fst (List.hd_exn sorted)
    | ch :: rest ->
        if Char.equal last_char ch then loop acc last_char (count + 1) rest
        else loop ((last_char, count) :: acc) ch 1 rest
  in
  loop [] ' ' 0 list

let solve_p1 lines =
  let length = String.length (List.hd_exn lines) in
  let char_lists = Array.create ~len:length [] in

  let rec loop lines =
    match lines with
    | [] -> char_lists
    | line :: rest ->
        let lst = String.to_list line in
        List.iteri lst ~f:(fun idx ch ->
            let curr = Array.get char_lists idx in
            Array.set char_lists idx (ch :: curr));
        loop rest
  in
  let lists = loop lines in
  Array.map lists ~f:(fun list -> get_most_used_char list)

let res = solve_p1 aoc_input
let resultP1 = String.of_array res

let get_least_used_char ch_list =
  let list = List.sort ch_list ~compare:Char.compare in
  let rec loop acc last_char count list =
    match list with
    | [] ->
        let sorted =
          List.sort ((last_char, count) :: acc) ~compare:(fun a b ->
              Int.compare (snd a) (snd b))
        in
        let without_space = List.drop sorted 1 in
        without_space
    | ch :: rest ->
        if Char.equal last_char ch then loop acc last_char (count + 1) rest
        else loop ((last_char, count) :: acc) ch 1 rest
  in
  let lst = loop [] ' ' 0 list in
  fst (List.hd_exn lst)

let solve_p2 lines =
  let length = String.length (List.hd_exn lines) in
  let char_lists = Array.create ~len:length [] in

  let rec loop lines =
    match lines with
    | [] -> char_lists
    | line :: rest ->
        let lst = String.to_list line in
        List.iteri lst ~f:(fun idx ch ->
            let curr = Array.get char_lists idx in
            Array.set char_lists idx (ch :: curr));
        loop rest
  in
  let lists = loop lines in
  Array.map lists ~f:(fun list -> get_least_used_char list)

let resultP2 = String.of_array (solve_p2 aoc_input)
