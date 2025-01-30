open Core

let isExample = false
let debugflag = false

type state_t = { floors : string list array; elevator_floor : int }

(* Define the priority module *)
module IntPriority = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let hash = Hashtbl.hash
end

(* Define the value module *)
module State = struct
  type t = state_t

  let compare = Stdlib.compare
  let equal = ( = )
  let hash = Hashtbl.hash
end

module StatePSQ = Psq.Make (State) (IntPriority)

let floors =
  if isExample then [| [ "HYM"; "LIM" ]; [ "HYG" ]; [ "LIG" ]; [] |]
  else
    [|
      [ "POG"; "THG"; "THM"; "PRG"; "RUG"; "RUM"; "COG"; "COM" ];
      [ "POM"; "PRM" ];
      [];
      [];
    |]

let start_state = { floors; elevator_floor = 0 }

let string_of_floor floor floor_no =
  let s =
    string_of_int floor_no ^ "%"
    ^ List.fold floor ~init:"" ~f:(fun acc el -> acc ^ el ^ "/")
  in
  if List.is_empty floor then s else String.drop_suffix s 1

let string_of_floors floors =
  let s =
    Array.foldi floors ~init:"" ~f:(fun idx acc floor ->
        acc ^ string_of_floor floor idx ^ "&")
  in
  String.drop_suffix s 1

let string_of_state state =
  string_of_int state.elevator_floor ^ "|" ^ string_of_floors state.floors

let is_mc s = Char.equal (String.get s 2) 'M'
let is_gen s = Char.equal (String.get s 2) 'G'

let is_pair el1 el2 =
  String.equal (String.sub ~pos:0 ~len:2 el1) (String.sub ~pos:0 ~len:2 el2)

let has_single s_list is_fn =
  let rec loop s_list =
    match s_list with
    | [] -> false
    | el :: [] -> is_fn el
    | el1 :: el2 :: rest ->
        (* Printf.printf "%s :: %s\n" el1 el2;
           List.iter rest ~f:(fun el -> Printf.printf " :: %s" el); *)
        if is_pair el1 el2 then loop rest else is_fn el1 || is_fn el2
  in
  loop s_list

let has_single_mc s_list = has_single s_list is_mc
let has_single_gen s_list = has_single s_list is_gen
let has_gen s_list = List.exists s_list ~f:is_gen

let is_legal_floor floor =
  if List.length floor < 2 then true
  else
    let sorted = List.sort floor ~compare:String.compare in
    if has_single_mc sorted && has_gen floor then false else true

let is_legal_pair lst =
  match lst with
  | [ a; b ] ->
      let mtrla = String.sub a ~pos:0 ~len:2 in
      let mtrlb = String.sub b ~pos:0 ~len:2 in
      let typea = String.get a 2 in
      let typeb = String.get b 2 in
      String.equal mtrla mtrlb || Char.equal typea typeb
  | _ -> failwith "Must be list with two elements"

let is_target state =
  let lower_floors = Array.sub state.floors ~pos:0 ~len:3 in
  not (Array.exists lower_floors ~f:(fun floor -> not (List.is_empty floor)))

let is_valid_state state =
  Array.for_all state.floors ~f:(fun floor -> is_legal_floor floor)

let pick_one lst =
  let rec loop acc lst =
    match lst with [] -> acc | hd :: tl -> loop ([ hd ] :: acc) tl
  in
  loop [] lst

let is_compatible_pair pair =
  match pair with
  | [ a; b ] ->
      String.equal (String.sub ~pos:0 ~len:2 a) (String.sub ~pos:0 ~len:2 b)
  | _ -> failwith "Has to be a pair"

let remove_extra_pairs lst =
  let rec loop acc lst has_compat =
    match lst with
    | [] -> acc
    | a :: rest ->
        if is_compatible_pair a then
          if has_compat then loop acc rest true else loop (a :: acc) rest true
        else loop (a :: acc) rest has_compat
  in
  loop [] lst false

let rec pick_two lst =
  let pairs =
    match lst with
    | [] -> []
    | x :: xs ->
        let pairs_with_x = List.map ~f:(fun y -> [ x; y ]) xs in
        pairs_with_x @ pick_two xs
  in
  let res = List.filter pairs ~f:is_legal_pair in
  let res = remove_extra_pairs res in
  if debugflag then
    List.iter res ~f:(fun pair ->
        Printf.printf "Pair %s : %s \n" (List.hd_exn pair) (List.last_exn pair));
  if debugflag then Printf.printf "\n";
  res

let remove_stuff stuff_to_remove stuff =
  List.filter stuff ~f:(fun el ->
      not (List.mem stuff_to_remove el ~equal:String.equal))

let move_stuff stuff_to_move state change =
  let stuff_on_this_floor = Array.get state.floors state.elevator_floor in
  List.map stuff_to_move ~f:(fun pick ->
      let new_stuff_on_this_floor = remove_stuff pick stuff_on_this_floor in
      let new_stuff_on_next_floor =
        pick @ Array.get state.floors (state.elevator_floor + change)
      in
      let new_floors = Array.copy state.floors in
      Array.set new_floors state.elevator_floor new_stuff_on_this_floor;
      Array.set new_floors
        (state.elevator_floor + change)
        new_stuff_on_next_floor;
      let new_state =
        { floors = new_floors; elevator_floor = state.elevator_floor + change }
      in
      new_state)

let are_floors_below_empty state =
  let rec is_empty floor =
    if floor < 0 then true
    else if not (List.is_empty state.floors.(floor)) then false
    else is_empty (floor - 1)
  in
  is_empty (state.elevator_floor - 1)

let get_neighbour_states state visited =
  let stuff_on_this_floor = Array.get state.floors state.elevator_floor in
  let singles_to_move = pick_one stuff_on_this_floor in
  let pairs_to_move = pick_two stuff_on_this_floor in

  let moves_up =
    if state.elevator_floor = 3 then []
    else move_stuff (pairs_to_move @ singles_to_move) state 1
  in

  let stuff_to_move_down =
    if are_floors_below_empty state then []
    else if state.elevator_floor = 0 then []
    else if List.is_empty singles_to_move then pairs_to_move
    else singles_to_move
  in
  let moves_dn = move_stuff stuff_to_move_down state (-1) in
  let n_states = moves_up @ moves_dn in
  let n_states = List.filter n_states ~f:(fun state -> is_valid_state state) in
  let size_before = List.length n_states in
  let res_states =
    List.filter n_states ~f:(fun state ->
        not (Set.mem visited (string_of_state state)))
  in
  let size_after = List.length res_states in
  if debugflag then
    Printf.printf "Filter out : %d states\n" (size_before - size_after);
  res_states

let get_state_with_lowest_cost queue =
  match queue with
  | [] -> failwith "Queue is empty"
  | _ ->
      (* Find the element with the lowest cost *)
      let el_no_to_remove, item =
        List.foldi queue
          ~init:(0, List.hd_exn queue)
          ~f:(fun idx (min_idx, (min_cost, _)) (cost, state) ->
            if cost < min_cost then (idx, (cost, state))
            else (min_idx, (min_cost, state)))
      in
      (* Create a new queue without the element *)
      let new_queue =
        List.filteri queue ~f:(fun idx _ -> idx <> el_no_to_remove)
      in
      (new_queue, item)

let print_state state cost =
  for floor = 4 downto 1 do
    let elev = if state.elevator_floor = floor - 1 then 'E' else '.' in
    let s = string_of_floor state.floors.(floor - 1) (floor - 1) in
    Printf.printf "%d  %c %s \n" floor elev s
  done;
  Printf.printf "Cost: %d\n\n" cost;
  Out_channel.flush stdout;
  let _ = In_channel.input_line In_channel.stdin in
  ()

let solve_p1 start_state =
  let queue = StatePSQ.empty in
  let queue = StatePSQ.add start_state 0 queue in
  let visited =
    Set.add (Set.empty (module String)) (string_of_state start_state)
  in

  let rec loop queue visited print_lim =
    if StatePSQ.is_empty queue then None
    else
      let popped = StatePSQ.pop queue in
      match popped with
      | None -> failwith "Cant be empty here"
      | Some ((state, cost'), queue') ->
          let q_size = StatePSQ.size queue' in
          let print_lim =
            if q_size > print_lim then (
              Printf.printf "QS: %d\n" q_size;
              Out_channel.flush stdout;
              print_lim + 100000)
            else print_lim
          in
          if debugflag then print_state state cost';
          if is_target state then Some cost'
          else
            let visited' = Set.add visited (string_of_state state) in
            let new_states = get_neighbour_states state visited' in
            let queue' =
              List.fold new_states ~init:queue' ~f:(fun acc st ->
                  StatePSQ.add st (cost' + 1) acc)
            in

            loop queue' visited' print_lim
  in
  loop queue visited 100000

let resultP1 = Option.value_exn (solve_p1 start_state)
let resultP2 = 0
