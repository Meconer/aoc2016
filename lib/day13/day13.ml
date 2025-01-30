open Core

let is_example = false
let debug_flag = false

(* Define the priority module *)
module IntPriority = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let hash = Hashtbl.hash
end

type coord = { x : int; y : int }

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

let string_of_state state = string_of_int state.x ^ ":" ^ string_of_int state.y

let get_binary number =
  let rec loop number =
    if number <= 1 then [ number ]
    else
      let half = number / 2 in
      let rest = number mod 2 in
      rest :: loop half
  in

  List.rev (loop number)

let is_odd n = n mod 2 = 1

let wall_func coord dfnumber =
  let x, y = (coord.x, coord.y) in
  let res = (x * x) + (3 * x) + (2 * x * y) + y + (y * y) in
  let res = res + dfnumber in
  let binary = get_binary res in
  let bit_one_count = List.count binary ~f:(fun bit -> bit = 1) in
  if is_odd bit_one_count then '#' else '.'

let print_board cols rows dfnumber =
  let rec row_loop row =
    let rec col_loop acc col =
      if col = cols then List.rev acc
      else
        let board_char = wall_func { x = col; y = row } dfnumber in
        col_loop (board_char :: acc) (col + 1)
    in

    if row = rows then ()
    else
      let c_list = col_loop [] 0 in
      let line = String.of_list c_list in
      Printf.printf "%s\n" line;
      row_loop (row + 1)
  in
  row_loop 0

let get_neighbour_states state df_number =
  let delta = [ (0, 1); (0, -1); (1, 0); (-1, 0) ] in
  let neighbours =
    List.map delta ~f:(fun d -> { x = state.x + fst d; y = state.y + snd d })
    |> List.filter ~f:(fun coord -> coord.x >= 0 && coord.y >= 0)
    |> List.filter ~f:(fun coord -> Char.equal (wall_func coord df_number) '.')
  in
  neighbours

let bfs start_state target_state df_number =
  let queue = StatePSQ.empty in
  let queue = StatePSQ.add start_state 0 queue in
  let visited =
    Set.add (Set.empty (module String)) (string_of_state start_state)
  in

  let rec loop queue visited =
    if StatePSQ.is_empty queue then None
    else
      let popped = StatePSQ.pop queue in
      match popped with
      | None -> failwith "Cant be empty here"
      | Some ((state, cost'), queue') ->
          if State.equal target_state state then Some cost'
          else
            let visited' = Set.add visited (string_of_state state) in
            let new_states = get_neighbour_states state df_number in
            let visited' =
              List.fold new_states ~init:visited' ~f:(fun acc st ->
                  Set.add acc (string_of_state st))
            in
            let queue' =
              List.fold new_states ~init:queue' ~f:(fun acc st ->
                  StatePSQ.add st (cost' + 1) acc)
            in

            loop queue' visited'
  in
  loop queue visited

let result_p1 = 0
let result_p2 = 0
