open Core

let isExample = false

let filename =
  if isExample then "lib/day10/example.txt" else "lib/day10/input.txt"

let aoc_input = In_channel.read_lines filename

type rule_t = { from_bot : string; to_high : string; to_low : string }

let parse_rules lines =
  let rec loop start_bots rules lines =
    match lines with
    | [] -> (start_bots, rules)
    | line :: rest ->
        if String.is_prefix line ~prefix:"value" then
          let value, bot =
            Scanf.sscanf line "value %d goes to bot %d" (fun a b ->
                (a, "bot" ^ string_of_int b))
          in
          let start_bots =
            Map.update start_bots bot ~f:(function
              | None -> [ value ]
              | Some values -> value :: values)
          in
          loop start_bots rules rest
        else if not (String.is_prefix line ~prefix:"bot") then
          failwith ("Error in line: " ^ line)
        else
          let from_bot, to_low, to_high =
            Scanf.sscanf line "%s %d gives low to %s %d and high to %s %d"
              (fun g gn l ln h hn ->
                ( g ^ string_of_int gn,
                  l ^ string_of_int ln,
                  h ^ string_of_int hn ))
          in
          let rules = { from_bot; to_high; to_low } :: rules in
          loop start_bots rules rest
  in
  loop (Map.empty (module String)) [] lines

let find_bot_with v1 v2 bots =
  Map.fold bots ~init:None ~f:(fun ~key:bot ~data:values acc ->
      if
        List.exists values ~f:(fun a -> a = v1)
        && List.exists values ~f:(fun a -> a = v2)
      then Some bot
      else acc)

let check_outputs_p2 bots =
  let out0 = Map.find bots "output0" in
  match out0 with
  | None -> None
  | Some out0 -> (
      let out1 = Map.find bots "output1" in
      match out1 with
      | None -> None
      | Some out1 -> (
          let out2 = Map.find bots "output2" in
          match out2 with
          | None -> None
          | Some out2 ->
              Some (List.hd_exn out0 * List.hd_exn out1 * List.hd_exn out2)))

let solve bots rules part2 =
  let rec loop bots rules =
    match rules with
    | [] -> (bots, false)
    | rule :: rest -> (
        let values_opt = Map.find bots rule.from_bot in
        match values_opt with
        | None -> loop bots rest
        | Some values ->
            if List.length values < 2 then loop bots rest
            else
              let a, b = (List.hd_exn values, List.last_exn values) in
              let high, low = (max a b, min a b) in
              let bots = Map.set bots ~key:rule.from_bot ~data:[] in
              let bots =
                Map.update bots rule.to_low ~f:(function
                  | None -> [ low ]
                  | Some values -> low :: values)
              in
              let bots =
                Map.update bots rule.to_high ~f:(function
                  | None -> [ high ]
                  | Some values -> high :: values)
              in
              if Option.is_some (find_bot_with 61 17 bots) then (bots, true)
              else loop bots rest)
  in

  let rec solve_p1_loop bots rules =
    let bots, found = loop bots rules in
    if found then
      match find_bot_with 61 17 bots with
      | None -> solve_p1_loop bots rules
      | Some bot -> (bot, 0)
    else solve_p1_loop bots rules
  in

  let rec solve_p2_loop bots rules =
    let bots, _ = loop bots rules in
    let p2_answer_opt = check_outputs_p2 bots in
    match p2_answer_opt with
    | None -> solve_p2_loop bots rules
    | Some p2_answer -> p2_answer
  in

  if not part2 then solve_p1_loop bots rules
  else
    let res = solve_p2_loop bots rules in
    ("", res)

let bots, rules = parse_rules aoc_input
let bot, _ = solve bots rules false

let resultP1 =
  int_of_string (String.sub bot ~pos:3 ~len:(String.length bot - 3))

let bots, rules = parse_rules aoc_input
let _, resultP2 = solve bots rules true
