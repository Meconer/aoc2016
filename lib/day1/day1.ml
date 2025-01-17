open Core

let isExample = false

let filename =
  if isExample then "lib/day1/example.txt" else "lib/day1/input.txt"

let aoc_input =
  if not isExample then In_channel.read_all filename else "R5, L5, R5, R3"

let instructions = String.split aoc_input ~on:',' |> List.map ~f:String.strip

type dirT = North | East | South | West
type turnT = Left | Right
type posT = { x : int; y : int }

let hash_of_pos pos = (pos.x * 100000) + pos.y

let pos_of_hash hash =
  let x = hash / 100000 in
  let y = hash mod 100000 in
  { x; y }

let turn_of_char ts =
  match ts with 'L' -> Left | 'R' -> Right | _ -> failwith "Invalid turn str"

let do_turn dir turn =
  match (turn, dir) with
  | Left, North -> West
  | Left, East -> North
  | Left, South -> East
  | Left, West -> South
  | Right, North -> East
  | Right, East -> South
  | Right, South -> West
  | Right, West -> North

let solve_p1 instructions =
  let rec loop pos dir instructions =
    match instructions with
    | [] -> pos
    | instruction :: rest -> (
        let turn = String.get instruction 0 |> turn_of_char in

        let steps =
          String.sub instruction ~pos:1 ~len:(String.length instruction - 1)
          |> int_of_string
        in
        let new_dir = do_turn dir turn in
        match new_dir with
        | North -> loop { pos with y = pos.y + steps } new_dir rest
        | South -> loop { pos with y = pos.y - steps } new_dir rest
        | West -> loop { pos with x = pos.x - steps } new_dir rest
        | East -> loop { pos with x = pos.x + steps } new_dir rest)
  in
  loop { x = 0; y = 0 } North instructions

let solve_p2 instructions =
  let p_set = Set.empty (module Int) in

  let rec loop pos dir p_set instructions =
    match instructions with
    | [] -> failwith "Didnt find answer"
    | instruction :: rest ->
        let turn = String.get instruction 0 |> turn_of_char in

        let steps =
          String.sub instruction ~pos:1 ~len:(String.length instruction - 1)
          |> int_of_string
        in
        let new_dir = do_turn dir turn in

        let rec do_steps pos steps_left p_set dir =
          if steps_left = 0 then (p_set, pos, false)
          else
            let new_pos =
              match new_dir with
              | North -> { pos with y = pos.y + 1 }
              | South -> { pos with y = pos.y - 1 }
              | West -> { pos with x = pos.x - 1 }
              | East -> { pos with x = pos.x + 1 }
            in
            if Set.mem p_set (hash_of_pos new_pos) then (p_set, new_pos, true)
            else
              let new_p_set = Set.add p_set (hash_of_pos new_pos) in
              do_steps new_pos (steps_left - 1) new_p_set dir
        in

        let new_p_set, new_pos, found_dup = do_steps pos steps p_set new_dir in
        if found_dup then new_pos else loop new_pos new_dir new_p_set rest
  in

  loop { x = 0; y = 0 } North p_set instructions

let manh_dist pos = abs pos.x + abs pos.y
let resultP1 = manh_dist (solve_p1 instructions)
let resultP2 = manh_dist (solve_p2 instructions)
