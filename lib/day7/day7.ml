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

let split_line line =
  let rec loop in_bracket_parts out_bracket_parts line =
    let fb_pos = String.index line '[' in
    let sb_pos = String.index line ']' in
    match fb_pos with
    | None -> (in_bracket_parts, out_bracket_parts ^ "___" ^ line)
    | Some idx -> (
        match sb_pos with
        | None -> failwith "No matching bracket"
        | Some s_idx ->
            let out_bracket =
              out_bracket_parts ^ "___" ^ String.sub line ~pos:0 ~len:idx
            in
            let in_bracket =
              in_bracket_parts ^ "___"
              ^ String.sub line ~pos:(idx + 1) ~len:(s_idx - idx - 1)
            in
            loop in_bracket out_bracket
              (String.sub line ~pos:(s_idx + 1)
                 ~len:(String.length line - s_idx - 1)))
  in
  loop "" "" line

let supports_TLS line =
  let in_bracket, out_bracket = split_line line in
  if has_rev_pair in_bracket then false else has_rev_pair out_bracket

let get_triplets s =
  let l = String.to_list s in
  let rec loop acc lst =
    match lst with
    | [ _; _ ] | _ :: [] | [] -> acc
    | a :: b :: c :: rest ->
        if Char.equal a c && not (Char.equal a b) then
          let triplet = String.of_list [ a; b; c ] in
          loop (triplet :: acc) (b :: c :: rest)
        else loop acc (b :: c :: rest)
  in
  loop [] l

let get_corresponding_bab triplet =
  let l = String.to_list triplet in
  let a = List.hd_exn l in
  let b = List.nth_exn l 1 in
  String.of_list [ b; a; b ]

let supports_SSL line =
  let in_bracket, out_bracket = split_line line in
  let triplets = get_triplets out_bracket in
  List.exists triplets ~f:(fun triplet ->
      let corresponding_bab = get_corresponding_bab triplet in
      String.is_substring in_bracket ~substring:corresponding_bab)

let solve_p1 lines = List.count lines ~f:(fun line -> supports_TLS line)
let resultP1 = solve_p1 aoc_input
let solve_p2 lines = List.count lines ~f:(fun line -> supports_SSL line)
let resultP2 = solve_p2 aoc_input
