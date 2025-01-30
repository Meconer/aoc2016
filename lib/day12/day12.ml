open Core

let isExample = false

let filename =
  if isExample then "lib/day12/example.txt" else "lib/day12/input.txt"

let aoc_input = In_channel.read_lines filename
let program = Array.of_list aoc_input

type reg_t = { a : int; b : int; c : int; d : int }

let get_reg_value reg regs =
  match reg with
  | "a" -> regs.a
  | "b" -> regs.b
  | "c" -> regs.c
  | "d" -> regs.d
  | _ -> failwith "illegal register"

let set_reg_value reg regs value =
  match reg with
  | "a" -> { regs with a = value }
  | "b" -> { regs with b = value }
  | "c" -> { regs with c = value }
  | "d" -> { regs with d = value }
  | _ -> failwith "illegal register"

let do_cpy line regs =
  match Scanf.sscanf_opt line "cpy %s %s" (fun x y -> (x, y)) with
  | None -> failwith "Wrong format for cpy"
  | Some (x, y) ->
      let number =
        match int_of_string_opt x with
        | None -> get_reg_value x regs
        | Some value -> value
      in
      set_reg_value y regs number

let do_inc line regs =
  match Scanf.sscanf_opt line "inc %s" (fun x -> x) with
  | None -> failwith "Wrong format for inc"
  | Some x -> set_reg_value x regs (get_reg_value x regs + 1)

let do_dec line regs =
  match Scanf.sscanf_opt line "dec %s" (fun x -> x) with
  | None -> failwith "Wrong format for dec"
  | Some x -> set_reg_value x regs (get_reg_value x regs - 1)

let do_jnz line regs ip =
  match Scanf.sscanf_opt line "jnz %s %d" (fun x y -> (x, y)) with
  | None -> failwith "Wrong format for jnz"
  | Some (x, y) ->
      let number =
        match int_of_string_opt x with
        | None -> get_reg_value x regs
        | Some value -> value
      in
      if number = 0 then (ip + 1, regs) else (ip + y, regs)

let exec_line line ip regs =
  match line with
  | line when String.is_prefix line ~prefix:"cpy" ->
      let regs = do_cpy line regs in
      (ip + 1, regs)
  | line when String.is_prefix line ~prefix:"inc" ->
      let regs = do_inc line regs in
      (ip + 1, regs)
  | line when String.is_prefix line ~prefix:"dec" ->
      let regs = do_dec line regs in
      (ip + 1, regs)
  | line when String.is_prefix line ~prefix:"jnz" ->
      let ip, regs = do_jnz line regs ip in
      (ip, regs)
  | _ -> failwith "Illegal instruction"

let solve program regs =
  let rec loop ip regs =
    if ip < 0 || ip >= Array.length program then regs
    else
      let curr_line = program.(ip) in
      (* Printf.printf "ip: %d   %s\n" ip curr_line; *)
      let ip, regs = exec_line curr_line ip regs in
      loop ip regs
  in
  loop 0 regs

let p1_regs = solve program { a = 0; b = 0; c = 0; d = 0 }
let resultP1 = p1_regs.a
let p2_regs = solve program { a = 0; b = 0; c = 1; d = 0 }
let resultP2 = p2_regs.a
