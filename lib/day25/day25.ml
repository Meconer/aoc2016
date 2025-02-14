open Core

let isExample = false

let filename =
  if isExample then "lib/day23/example.txt" else "lib/day23/input.txt"

let aoc_input = In_channel.read_lines filename
let program = Array.of_list aoc_input

type reg_t = { a : int; b : int; c : int; d : int }

let print_regs regs =
  Printf.printf "a: %d b: %d c: %d d: %d\n" regs.a regs.b regs.c regs.d

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
  match Scanf.sscanf_opt line "jnz %s %s" (fun x y -> (x, y)) with
  | None -> failwith "Wrong format for jnz"
  | Some (x, y) ->
      let number =
        match int_of_string_opt x with
        | None -> get_reg_value x regs
        | Some value -> value
      in
      let jmp =
        match int_of_string_opt y with
        | None -> get_reg_value y regs
        | Some value -> value
      in
      if number = 0 then (ip + 1, regs) else (ip + jmp, regs)

let do_tgl line regs ip program =
  match Scanf.sscanf_opt line "tgl %s" (fun x -> x) with
  | None -> failwith "Wrong format for tgl"
  | Some x -> (
      let ip_to_change = ip + get_reg_value x regs in
      Printf.printf "ip_to_change %d\n" ip_to_change;
      if ip_to_change < 0 || ip_to_change >= Array.length program then
        (ip, regs, program)
      else
        let instr_line_to_toggle = program.(ip_to_change) in
        (* Printf.printf "instr_line_to_toggle %s\n" instr_line_to_toggle; *)
        match instr_line_to_toggle with
        | line when String.is_prefix line ~prefix:"cpy" ->
            let n_line = "jnz" ^ String.chop_prefix_exn line ~prefix:"cpy" in
            Array.set program ip_to_change n_line;
            (ip, regs, program)
        | line when String.is_prefix line ~prefix:"inc" ->
            let n_line = "dec" ^ String.chop_prefix_exn line ~prefix:"inc" in
            Array.set program ip_to_change n_line;
            (ip, regs, program)
        | line when String.is_prefix line ~prefix:"dec" ->
            let n_line = "inc" ^ String.chop_prefix_exn line ~prefix:"dec" in
            Array.set program ip_to_change n_line;
            (ip, regs, program)
        | line when String.is_prefix line ~prefix:"tgl" ->
            let n_line = "inc" ^ String.chop_prefix_exn line ~prefix:"tgl" in
            Array.set program ip_to_change n_line;
            (ip, regs, program)
        | line when String.is_prefix line ~prefix:"jnz" ->
            let n_line = "cpy" ^ String.chop_prefix_exn line ~prefix:"jnz" in
            Array.set program ip_to_change n_line;
            (ip, regs, program)
        | _ -> failwith "Illegal instr")

let print_status line ip regs program =
  Printf.printf "----\nPL: %s\n" line;
  print_regs regs;
  Printf.printf "IP: %d\n" ip;
  Array.iter program ~f:(fun l -> Printf.printf "%s\n" l);
  Out_channel.flush stdout;
  (* let _ = In_channel.input_line In_channel.stdin in *)
  ()

let exec_line line ip regs program =
  (* print_status line ip regs program; *)
  (* print_regs regs; *)
  match line with
  | line when String.is_prefix line ~prefix:"cpy" ->
      let regs = do_cpy line regs in
      (ip + 1, regs, program)
  | line when String.is_prefix line ~prefix:"inc" ->
      let regs = do_inc line regs in
      (ip + 1, regs, program)
  | line when String.is_prefix line ~prefix:"dec" ->
      let regs = do_dec line regs in
      (ip + 1, regs, program)
  | line when String.is_prefix line ~prefix:"jnz" ->
      let ip, regs = do_jnz line regs ip in
      (ip, regs, program)
  | line when String.is_prefix line ~prefix:"tgl" ->
      let ip, regs, program = do_tgl line regs ip program in
      (ip + 1, regs, program)
  | _ -> failwith "Illegal instruction"

let solve program regs =
  let rec loop ip regs program =
    if ip < 0 || ip >= Array.length program then regs
    else
      let curr_line = program.(ip) in
      (* Printf.printf "ip: %d   %s\n" ip curr_line; *)
      let ip, regs, program = exec_line curr_line ip regs program in
      loop ip regs program
  in
  loop 0 regs program

let p1_regs = solve program { a = 7; b = 0; c = 0; d = 0 }
let result_p1 = p1_regs.a
let program = Array.of_list aoc_input
let p2_regs = solve program { a = 12; b = 0; c = 0; d = 0 }
let result_p2 = p2_regs.a
