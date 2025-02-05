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
  type t = coord

  let compare a b =
    let cmp = Int.compare a.x b.x in
    if cmp = 0 then Int.compare a.y b.y else cmp

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

let get_neighbour_states path pos =
  let hash = get_hash path in
  let neighbours =
    String.sub hash ~pos:0 ~len:4
    |> String.to_list
    |> List.mapi ~f:(fun idx ch -> (new_pos pos idx, is_open ch))
    |> List.filter ~f:(fun (pos, is_open) ->
           pos.x >= 0 && pos.x < 4 && pos.y >= 0 && pos.y < 4 && is_open)
  in
  neighbours

(* let solve start target pass_code =
  let path = pass_code in
  let queue = StatePSQ.( (add start 0) empty)  in
    let rec loop queue =
      if StatePSQ.is_empty queue then None
      else
        let popped = StatePSQ.pop queue in
        match popped with 
        | None -> failwith "Empty! Wtf?"
        | Some ((pos, dist), queue) ->
          if State.equal pos target then
            path
          else
            let neighbours = get_neighbour_states path pos
          in


let result_p1 = ""
let result_p2 = "" *)
