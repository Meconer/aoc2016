open Core

let isExample = true

type state_t = { floors : string list array; elevator_floor : int }

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
  string_of_int state.elevator_floor ^ "|" ^ string_of_floors floors

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

let legal_floor floor =
  if List.length floor < 2 then true
  else
    let sorted = List.sort floor ~compare:String.compare in
    if has_single_mc sorted && has_single_gen sorted then false else true

let is_target state =
  let lower_floors = Array.sub state.floors ~pos:0 ~len:3 in
  not (Array.exists lower_floors ~f:(fun floor -> not (List.is_empty floor)))

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

(* let get_neighbour_states state =
   let stuff_on_this_floor = List.nth_exn state.floors state.elevator_floor in
   let stuff_to_move = pick_one stuff_on_this_floor @ pick_two stuff_on_this_floor in
   if state.elevator_floor < 3 then
     (* Not on top floor, We can move stuff up *)

   let moves_up = List.map stuff_to_move ~f:(fun pick ->
      let new_stuff_on_this_floor = remove_stuff pick stuff_on_this_floor in
    ) *)

(* let solve_p1 floors elevator =

     let rec loop step_count state =
       if is_target state then step_count
       else

   let resultP1 = 0
   let resultP2 = 0 *)
