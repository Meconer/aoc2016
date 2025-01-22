open Core

let isExample = true

let filename =
  if isExample then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_all filename

let solve_p1 line =
  let rec loop s_before s_after =
    let p_pos1 = String.index s_after '(' in
    match p_pos1 with
    | None -> s_before ^ s_after
    | Some p_pos1 -> (
        let p_pos2 = String.index s_after ')' in
        match p_pos2 with
        | None -> s_before ^ s_after
        | Some p_pos2 ->
            let new_s_before =
              s_before ^ String.sub s_after ~pos:0 ~len:p_pos1
            in
            let cp_instr =
              String.sub s_after ~pos:(p_pos1 + 1) ~len:(p_pos2 - p_pos1 - 1)
            in
            new_s_before ^ " - " ^ cp_instr)
  in
  loop "" line

let resultP1 = 0
let resultP2 = 0
