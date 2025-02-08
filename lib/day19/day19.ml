open Core

let is_example = false
let no_of_elfs = if is_example then 5 else 3014387

type node_t = { idx : int; count : int }

let elfs = ref (Array.init no_of_elfs ~f:(fun i -> { idx = i + 1; count = 1 }))

let rec get_next_elfs_present start_pos pos =
  if pos = start_pos then None
  else
    let next_elf = !elfs.(pos) in
    if next_elf.count = 0 then
      get_next_elfs_present start_pos ((pos + 1) mod no_of_elfs)
    else (
      Array.set !elfs pos { idx = pos; count = 0 };
      Some next_elf.count)

let solve elfs start =
  let rec loop pos =
    let this_elf = elfs.(pos) in
    (* Array.iter elfs ~f:(fun el ->
           Printf.printf "idx %d, c: %d\n" el.idx el.count);
       Out_channel.flush stdout; *)
    (* let _ = In_channel.input_line In_channel.stdin in *)
    if this_elf.count = 0 then loop ((pos + 1) mod no_of_elfs)
    else
      let next_count = get_next_elfs_present pos ((pos + 1) mod no_of_elfs) in
      match next_count with
      | None -> elfs.(pos).idx
      | Some next_count ->
          let this_elf =
            { this_elf with count = this_elf.count + next_count }
          in
          Array.set elfs pos this_elf;
          loop ((pos + 1) mod no_of_elfs)
  in
  loop start

let result_p1 = 0
(* let result_p1 = solve !elfs 0 *)

(* Reset elfs *)
let elfs = ref (Array.init no_of_elfs ~f:(fun i -> { idx = i + 1; count = 1 }))

let get_steal_pos elfs pos =
  let no_of_elfs = Array.length elfs in
  let elf_count = Array.count elfs ~f:(fun el -> el.count > 0) in
  if elf_count mod 1000 = 0 then (
    Printf.printf "Count: %d\n" elf_count;
    Out_channel.flush stdout);

  if elf_count = 1 then None
  else
    let count_forward = elf_count / 2 in

    let rec loop pos count =
      if count = 0 then
        if elfs.(pos).count > 0 then Some pos
        else loop ((pos + 1) mod no_of_elfs) count
      else if elfs.(pos).count = 0 then loop ((pos + 1) mod no_of_elfs) count
      else loop ((pos + 1) mod no_of_elfs) (count - 1)
    in
    let res = loop pos count_forward in
    res

let solve_p2 no_of_elfs start =
  let elfs = Array.init no_of_elfs ~f:(fun i -> { idx = i + 1; count = 1 }) in
  let rec loop pos =
    (* Printf.printf "Pos: %d\n" pos; *)
    let this_elf = elfs.(pos) in
    (* Array.iteri elfs ~f:(fun i el ->
           Printf.printf "i:%d : idx %d, c: %d\n" i el.idx el.count);
       Out_channel.flush stdout;
       let _ = In_channel.input_line In_channel.stdin in *)
    if this_elf.count = 0 then loop ((pos + 1) mod no_of_elfs)
    else
      let steal_pos_opt = get_steal_pos elfs pos in

      match steal_pos_opt with
      | None -> elfs.(pos).idx
      | Some steal_pos ->
          (* Printf.printf "Steal pos : %d\n" steal_pos; *)
          let elf_to_steal_from = elfs.(steal_pos) in
          let this_elf =
            { this_elf with count = this_elf.count + elf_to_steal_from.count }
          in
          let elf_to_steal_from = { elf_to_steal_from with count = 0 } in
          Array.set elfs pos this_elf;
          Array.set elfs steal_pos elf_to_steal_from;
          (* let next_pos = (pos + 1) mod no_of_elfs in
             Printf.printf "Next: %d\n" next_pos; *)
          loop ((pos + 1) mod no_of_elfs)
  in
  loop start

let rec loop count =
  if count = 800 then ()
  else
    let winner = solve_p2 count 0 in
    Printf.printf "C: %d - Winner %d\n" count winner;
    loop (count + 1)
(* Pattern
3^0 1 1
3^1 3 
*)

let predict_winner no_of_elfs =
  if 
let x = solve_p2 5 0
let result_p2 = x
