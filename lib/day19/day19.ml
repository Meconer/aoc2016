open Core

let is_example = true
let no_of_elfs = if is_example then 5 else 3014387

type node_t = { idx : int; count : int }

let elfs = ref (Array.init no_of_elfs ~f:(fun idx -> { idx; count = 1 }))

let rec get_next_elfs_present start_pos pos =
  if pos = start_pos then None
  else
    let next_elf = !elfs.(pos) in
    if next_elf.count = 0 then get_next_elfs_present start_pos (pos + 1)
    else (
      Array.set !elfs pos { idx = pos; count = 0 };
      Some next_elf.count)

let solve elfs start =
  let rec loop pos =
    let this_elf = elfs.(pos) in
    if this_elf.count = 0 then loop ((pos + 1) mod no_of_elfs)
    else
      let next_count = get_next_elfs_present pos ((pos + 1) mod no_of_elfs) in
      match next_count with
      | None -> pos
      | Some next_count ->
          let this_elf =
            { this_elf with count = this_elf.count + next_count }
          in
          Array.set elfs pos this_elf;
          loop ((pos + 1) mod no_of_elfs)
  in
  loop start

let result_p1 = 0
let result_p2 = 0
