open Core

let is_example = false
let pass_code = if is_example then "ihgpwlah" else "lpvhkcbi"

type coord = { x : int; y : int }

(* Define the priority module *)
module IntPriority = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let hash = Hashtbl.hash
end

(* Define the value module *)
module State = struct
  type t = coord * string

  let compare a b =
    let cmp = Int.compare (fst a).x (fst b).x in
    if cmp = 0 then
      let cmp' = Int.compare (fst a).y (fst b).y in
      if cmp' = 0 then String.compare (snd a) (snd b) else cmp'
    else cmp

  let equal a b = a.x = b.x && a.y = b.y
  let hash = Hashtbl.hash
end

module StatePSQ = Psq.Make (State) (IntPriority)

let get_hash s = Md5.to_hex (Md5.digest_string s)

let is_open ch =
  if Char.is_digit ch || Char.equal ch 'a' then false
  else if Char.( >= ) ch 'b' && Char.( <= ) ch 'f' then true
  else false

let new_pos pos idx =
  match idx with
  | 0 -> { x = pos.x; y = pos.y - 1 } (* Up *)
  | 1 -> { x = pos.x; y = pos.y + 1 } (* Down *)
  | 2 -> { x = pos.x - 1; y = pos.y } (* Left *)
  | 3 -> { x = pos.x + 1; y = pos.y } (* Right *)
  | _ -> failwith "Illegal idx"

let width = 4
let height = 4

let add_dir_to_path path idx =
  let char_to_add =
    match idx with
    | 0 -> "U" (* Up *)
    | 1 -> "D" (* Down *)
    | 2 -> "L" (* Left *)
    | 3 -> "R" (* Right *)
    | _ -> failwith "Illegal idx"
  in
  path ^ char_to_add

let get_neighbour_states path pos =
  let hash = get_hash path in
  let neighbours =
    String.sub hash ~pos:0 ~len:4
    |> String.to_list
    |> List.mapi ~f:(fun idx ch ->
           let new_path = add_dir_to_path path idx in
           (new_pos pos idx, is_open ch, new_path))
    |> List.filter ~f:(fun (pos, is_open, _) ->
           pos.x >= 0 && pos.x < width && pos.y >= 0 && pos.y < height
           && is_open)
  in
  neighbours

let solve start target pass_code =
  let path = pass_code in
  let queue = StatePSQ.((add (start, path) 0) empty) in

  let rec loop queue =
    if StatePSQ.is_empty queue then None
    else
      let popped = StatePSQ.pop queue in
      match popped with
      | None -> failwith "Empty! Wtf?"
      | Some (((pos, path), dist), queue) ->
          if State.equal pos target then Some path
          else
            let neighbours = get_neighbour_states path pos in
            let q =
              List.fold neighbours ~init:queue ~f:(fun q neighbour ->
                  let new_pos, _, new_path = neighbour in
                  StatePSQ.add (new_pos, new_path) (dist + 1) q)
            in
            loop q
  in

  loop queue

let start = { x = 0; y = 0 }
let target = { x = 3; y = 3 }

let res =
  match solve start target pass_code with
  | Some res ->
      let p_len = String.length pass_code in
      String.sub res ~pos:p_len ~len:(String.length res - p_len)
  | None -> "No result"

let result_p1 = res
let result_p2 = ""
