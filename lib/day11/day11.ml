open Core

let isExample = true

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

let is_legal_floor floor =
  if List.length floor < 2 then true
  else
    let sorted = List.sort floor ~compare:String.compare in
    if has_single_mc sorted && has_single_gen sorted then false else true

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

let rec pick_two lst =
  match lst with
  | [] -> []
  | x :: xs ->
      let pairs_with_x = List.map ~f:(fun y -> [ x; y ]) xs in
      pairs_with_x @ pick_two xs

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

let get_neighbour_states state visited =
  let stuff_on_this_floor = Array.get state.floors state.elevator_floor in
  let stuff_to_move =
    pick_one stuff_on_this_floor @ pick_two stuff_on_this_floor
  in

  let moves_up =
    if state.elevator_floor = 3 then [] else move_stuff stuff_to_move state 1
  in
  let moves_dn =
    if state.elevator_floor = 0 then [] else move_stuff stuff_to_move state (-1)
  in
  let n_states = moves_up @ moves_dn in
  let n_states = List.filter n_states ~f:(fun state -> is_valid_state state) in
  List.filter n_states ~f:(fun state ->
      not (Set.mem visited (string_of_state state)))

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

let solve_p1 start_state =
   let queue = StatePSQ.empty in
   let queue = StatePSQ.add 0 start_state queue in
   let visited =
     Set.add (Set.empty (module String)) (string_of_state start_state)
   in

   let rec loop queue visited =
     if StatePSQ.is_empty queue then None
     else
       let popped = StatePSQ. queue in
       match popped with
       | None -> failwith "Cant be empty here"
       | Some
       let queue, (cost', state) = StatePSQ.pop queue in
       if is_target state then Some cost'
       else
         let visited = Set.add visited (string_of_state state) in
         let new_states = get_neighbour_states state visited in
         List.iter new_states ~f:(fun el ->
             Printf.printf "NS:  %s\n" (string_of_state el));
         let items_to_append =
           List.map new_states ~f:(fun st -> (cost' + 1, st))
         in
         List.iter items_to_append ~f:(fun el ->
             Printf.printf "Add: %d, %s\n" (fst el) (string_of_state (snd el)));
         let queue' = queue @ items_to_append in
         List.iter queue' ~f:(fun el ->
             Printf.printf "Q: %d, %s\n" (fst el) (string_of_state (snd el)));
         Out_channel.flush stdout;
         let _ = In_channel.input_line In_channel.stdin in
         ();

         loop queue' visited
   in
   loop queue visited

let resultP1 = 0
let resultP2 = 0
