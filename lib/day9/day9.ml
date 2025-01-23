open Core

let isExample = false

let filename =
  if isExample then "lib/day9/example.txt" else "lib/day9/input.txt"

let aoc_input = In_channel.read_all filename

let repeat_str s count =
  let rec loop acc count =
    if count = 1 then acc else loop (acc ^ s) (count - 1)
  in
  loop s count

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
            let no_of_chars, repeats =
              Scanf.sscanf cp_instr "%dx%d" (fun a b -> (a, b))
            in
            let str_to_repeat =
              String.sub s_after ~pos:(p_pos2 + 1) ~len:no_of_chars
            in
            let rep_str = repeat_str str_to_repeat repeats in
            let start_of_rest_of_str = p_pos2 + 1 + no_of_chars in
            let new_s_after =
              String.sub s_after ~pos:start_of_rest_of_str
                ~len:(String.length s_after - start_of_rest_of_str)
            in
            loop (new_s_before ^ rep_str) new_s_after)
  in
  loop "" line

let resultP1 = String.length (solve_p1 aoc_input)

let solve_p2 line =
  let memo = Map.empty (module String) in
  let rec loop length s_after memo =
    if Map.mem memo s_after then Map.find_exn memo s_after
    else
      let p_pos1 = String.index s_after '(' in
      match p_pos1 with
      | None ->
          (* Printf.printf "%s\n" s_after; *)
          length + String.length s_after
      | Some p_pos1 -> (
          let p_pos2 = String.index s_after ')' in
          match p_pos2 with
          | None -> length + String.length s_after
          | Some p_pos2 ->
              let cp_instr =
                String.sub s_after ~pos:(p_pos1 + 1) ~len:(p_pos2 - p_pos1 - 1)
              in
              let no_of_chars, repeats =
                Scanf.sscanf cp_instr "%dx%d" (fun a b -> (a, b))
              in
              let str_to_repeat =
                String.sub s_after ~pos:(p_pos2 + 1) ~len:no_of_chars
              in
              let rep_str = repeat_str str_to_repeat repeats in
              let start_of_rest_of_str = p_pos2 + 1 + no_of_chars in
              let new_s_after =
                String.sub s_after ~pos:start_of_rest_of_str
                  ~len:(String.length s_after - start_of_rest_of_str)
              in
              let length_to_add = loop 0 rep_str memo in
              let memo = Map.set memo ~key:rep_str ~data:length_to_add in
              let length = length + p_pos1 in
              loop (length + length_to_add) new_s_after memo)
  in
  loop 0 line memo

let resultP2 = solve_p2 aoc_input
