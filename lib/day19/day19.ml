open Core

let is_example = false
let no_of_elfs = if is_example then 17 else 3014387

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

let result_p1 = solve !elfs 0
let result_p2 = 0
