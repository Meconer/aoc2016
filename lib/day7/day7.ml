open Core

let isExample = false

let filename =
  if isExample then "lib/day7/example.txt" else "lib/day7/input.txt"

let aoc_input = In_channel.read_lines filename

let has_rev_pair s =
  let c_list = String.to_list s in
  let rec loop c_list =
    match c_list with
    | [ _; _; _ ] | [ _; _ ] | _ :: [] | [] -> false
    | a :: b :: c :: d :: rest ->
        if Char.equal a d && Char.equal b c && not (Char.equal a b) then true
        else loop (b :: c :: d :: rest)
  in
  loop c_list

let supports_TLS line =
  let rec loop in_bracket_parts out_bracket_parts line =
    let fb_pos = String.index line '[' in
    let sb_pos = String.index line ']' in
    match fb_pos with
    | None -> (in_bracket_parts, out_bracket_parts ^ "," ^ line)
    | Some idx -> (
        match sb_pos with
        | None -> failwith "No matching bracket"
        | Some s_idx ->
            let out_bracket =
              out_bracket_parts ^ "," ^ String.sub line ~pos:0 ~len:idx
            in
            let in_bracket =
              in_bracket_parts ^ ","
              ^ String.sub line ~pos:(idx + 1) ~len:(s_idx - idx - 1)
            in
            loop in_bracket out_bracket
              (String.sub line ~pos:(s_idx + 1)
                 ~len:(String.length line - s_idx - 1)))
  in
  let in_bracket, out_bracket = loop "" "" line in
  if has_rev_pair in_bracket then false else has_rev_pair out_bracket

let solve_p1 lines = List.count lines ~f:(fun line -> supports_TLS line)
let resultP1 = solve_p1 aoc_input
let resultP2 = 0
