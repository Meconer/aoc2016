open Core

let isExample = false

let filename =
  if isExample then "lib/day3/example.txt" else "lib/day3/input.txt"

let aoc_input = In_channel.read_lines filename

let is_valid line =
  let vals =
    Scanf.sscanf line " %d %d %d" (fun a b c -> [ a; b; c ])
    |> List.sort ~compare:Int.compare
  in
  match vals with a :: b :: c :: _ -> a + b > c | _ -> failwith "three ints"

let solve_p1 lines = List.count lines ~f:is_valid
let resultP1 = solve_p1 aoc_input

let count_valid lst =
  let digits =
    List.map lst ~f:(fun line ->
        Scanf.sscanf line " %d %d %d" (fun a b c -> [| a; b; c |]))
  in
  match digits with
  | a1 :: a2 :: a3 :: _ ->
      let tr1 = [ a1.(0); a2.(0); a3.(0) ] in
      let tr2 = [ a1.(1); a2.(1); a3.(1) ] in
      let tr3 = [ a1.(2); a2.(2); a3.(2) ] in
      List.count [ tr1; tr2; tr3 ] ~f:(fun tr ->
          let trs = List.sort tr ~compare:Int.compare in
          match trs with
          | a :: b :: c :: _ -> a + b > c
          | _ -> failwith "Invalid tr")
  | _ -> failwith "Invalid digits"

let solve_p2 lines =
  let rec loop acc lines =
    match lines with
    | l1 :: l2 :: l3 :: rest -> loop (acc + count_valid [ l1; l2; l3 ]) rest
    | [] -> acc
    | _ -> failwith "Wrong no of lines"
  in
  loop 0 lines

let resultP2 = solve_p2 aoc_input
