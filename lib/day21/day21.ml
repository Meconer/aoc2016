open Core

let isExample = false

let filename =
  if isExample then "lib/day21/example.txt" else "lib/day21/input.txt"

let password = if isExample then "abcde" else "abcdefgh"
let aoc_input = In_channel.read_lines filename

type commands_t =
  | Swap_Pos
  | Swap_Letter
  | Rotate_Left
  | Rotate_Right
  | Rotate_Pos
  | Reverse_Pos
  | Move_Pos

type command = {
  command : commands_t;
  pos_a : int;
  pos_b : int;
  letter_a : char;
  letter_b : char;
  no_steps : int;
}

let command_of_line line =
  match line with
  | line when String.is_prefix line ~prefix:"swap position" ->
      let pos_a, pos_b =
        Scanf.sscanf line "swap position %d with position %d" (fun a b ->
            (a, b))
      in
      {
        command = Swap_Pos;
        pos_a;
        pos_b;
        letter_a = 'X';
        letter_b = 'X';
        no_steps = 0;
      }
  | line when String.is_prefix line ~prefix:"swap letter" ->
      let letter_a, letter_b =
        Scanf.sscanf line "swap letter %c with letter %c" (fun a b -> (a, b))
      in
      {
        command = Swap_Letter;
        pos_a = 0;
        pos_b = 0;
        letter_a;
        letter_b;
        no_steps = 0;
      }
  | line when String.is_prefix line ~prefix:"rotate left" ->
      let no_steps = Scanf.sscanf line "rotate left %d step" (fun s -> s) in
      {
        command = Rotate_Left;
        pos_a = 0;
        pos_b = 0;
        letter_a = 'X';
        letter_b = 'X';
        no_steps;
      }
  | line when String.is_prefix line ~prefix:"rotate right" ->
      let no_steps = Scanf.sscanf line "rotate right %d step" (fun s -> s) in
      {
        command = Rotate_Right;
        pos_a = 0;
        pos_b = 0;
        letter_a = 'X';
        letter_b = 'X';
        no_steps;
      }
  | line when String.is_prefix line ~prefix:"rotate based" ->
      let letter_a =
        Scanf.sscanf line "rotate based on position of letter %c" (fun s -> s)
      in
      {
        command = Rotate_Pos;
        pos_a = 0;
        pos_b = 0;
        letter_a;
        letter_b = 'X';
        no_steps = 0;
      }
  | line when String.is_prefix line ~prefix:"move position" ->
      let pos_a, pos_b =
        Scanf.sscanf line "move position %d to position %d" (fun a b -> (a, b))
      in
      {
        command = Move_Pos;
        pos_a;
        pos_b;
        letter_a = 'X';
        letter_b = 'X';
        no_steps = 0;
      }
  | line when String.is_prefix line ~prefix:"reverse positions" ->
      let pos_a, pos_b =
        Scanf.sscanf line "reverse positions %d through %d" (fun a b -> (a, b))
      in
      {
        command = Reverse_Pos;
        pos_a;
        pos_b;
        letter_a = 'X';
        letter_b = 'X';
        no_steps = 0;
      }
  | _ -> failwith "Illegal command"

let swap_pos s pos_a pos_b =
  let ltr_a = String.get s pos_a in
  let ltr_b = String.get s pos_b in
  let s_mut = Bytes.of_string s in
  Bytes.set s_mut pos_b ltr_a;
  Bytes.set s_mut pos_a ltr_b;
  Bytes.to_string s_mut

let swap_letter s ltr_a ltr_b =
  let pos_a = String.index_exn s ltr_a in
  let pos_b = String.index_exn s ltr_b in
  let s_mut = Bytes.of_string s in
  Bytes.set s_mut pos_b ltr_a;
  Bytes.set s_mut pos_a ltr_b;
  Bytes.to_string s_mut

let rotate_left s n_steps =
  let s1 = String.sub s ~pos:0 ~len:n_steps in
  let s2 = String.sub s ~pos:n_steps ~len:(String.length s - n_steps) in
  s2 ^ s1

let rotate_right s n_steps =
  let l = String.length s in
  let n_steps = n_steps mod l in
  let p = l - n_steps in
  let s1 = String.sub s ~pos:0 ~len:p in
  let s2 = String.sub s ~pos:p ~len:(l - p) in
  s2 ^ s1

let rotate_on_letter s c =
  let idx = String.index_exn s c in
  let idx = if idx >= 4 then idx + 2 else idx + 1 in
  rotate_right s idx

let reverse s pos_a pos_b =
  let s1 = String.sub s ~pos:0 ~len:pos_a in
  let s3 = String.sub s ~pos:(pos_b + 1) ~len:(String.length s - pos_b - 1) in
  let s2 = String.sub s ~pos:pos_a ~len:(pos_b - pos_a + 1) in
  let s2 = String.rev s2 in
  s1 ^ s2 ^ s3

let move_pos s pos_a pos_b =
  if pos_b > pos_a then
    let s1 = String.sub s ~pos:0 ~len:pos_a in
    let sm = String.sub s ~pos:pos_a ~len:1 in
    let s2 = String.sub s ~pos:(pos_a + 1) ~len:(pos_b - pos_a) in
    let s3 = String.sub s ~pos:(pos_b + 1) ~len:(String.length s - pos_b - 1) in
    s1 ^ s2 ^ sm ^ s3
  else
    let s1 = String.sub s ~pos:0 ~len:pos_b in
    let sm = String.sub s ~pos:pos_a ~len:1 in
    let s2 = String.sub s ~pos:pos_b ~len:(pos_a - pos_b) in
    let s3 =
      if pos_a < String.length s - 1 then
        String.sub s ~pos:(pos_a + 1) ~len:(String.length s - pos_a - 1)
      else ""
    in
    s1 ^ sm ^ s2 ^ s3

let rotate_on_letter_rev s c =
  if String.length s <> 8 then failwith "Works only on str length of 8"
  else
    let idx = String.index_exn s c in
    let original_pos =
      if idx mod 2 = 1 then idx / 2 else if idx = 0 then 7 else 4 + (idx / 2)
    in
    if original_pos = 7 && idx = 0 then rotate_left s 1
    else if original_pos > idx then rotate_right s (original_pos - idx - 1)
    else rotate_left s (idx - original_pos)

let do_command command s =
  match command.command with
  | Swap_Pos -> swap_pos s command.pos_a command.pos_b
  | Swap_Letter -> swap_letter s command.letter_a command.letter_b
  | Rotate_Left -> rotate_left s command.no_steps
  | Rotate_Right -> rotate_right s command.no_steps
  | Rotate_Pos -> rotate_on_letter s command.letter_a
  | Reverse_Pos -> reverse s command.pos_a command.pos_b
  | Move_Pos -> move_pos s command.pos_a command.pos_b

let do_rev_command command s =
  match command.command with
  | Swap_Pos -> swap_pos s command.pos_a command.pos_b
  | Swap_Letter -> swap_letter s command.letter_a command.letter_b
  | Rotate_Left -> rotate_right s command.no_steps
  | Rotate_Right -> rotate_left s command.no_steps
  | Rotate_Pos -> rotate_on_letter_rev s command.letter_a
  | Reverse_Pos -> reverse s command.pos_a command.pos_b
  | Move_Pos -> move_pos s command.pos_b command.pos_a

let solve_p1 lines s =
  let rec loop s lines =
    match lines with
    | [] -> s
    | hd :: tl ->
        let command = command_of_line hd in
        let s = do_command command s in
        loop s tl
  in
  loop s lines

let result_p1 = solve_p1 aoc_input password

let solve_p2 lines s =
  let rec loop s lines =
    match lines with
    | [] -> s
    | hd :: tl ->
        let command = command_of_line hd in
        let s = do_rev_command command s in
        loop s tl
  in
  loop s (List.rev lines)

let result_p2 = solve_p2 aoc_input "fbgdceah"
