open Core

let isExample = true

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

let result_p1 = ""
let result_p2 = ""
