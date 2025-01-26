open Core

let isExample = true

type state_t = { floors : string list list; elevator_floor : int }

let floors =
  if isExample then [ [ "HYM"; "LIM" ]; [ "HYG" ]; [ "LIG" ]; [] ]
  else
    [
      [ "POG"; "THG"; "THM"; "PRG"; "RUG"; "RUM"; "COG"; "COM" ];
      [ "POM"; "PRM" ];
      [];
      [];
    ]

let start_state = { floors; elevator_floor = 0 }

let string_of_floor floor floor_no =
  let s =
    string_of_int floor_no ^ "%"
    ^ List.fold floor ~init:"" ~f:(fun acc el -> acc ^ el ^ "/")
  in
  if List.is_empty floor then s else String.drop_suffix s 1

let string_of_floors floors =
  let s =
    List.foldi floors ~init:"" ~f:(fun idx acc floor ->
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
  let lower_floors = List.drop_last_exn state.floors in
  not (List.exists lower_floors ~f:(fun floor -> not (List.is_empty floor)))

let pick_one lst =
  let rec loop acc lst =
    match lst with [] -> acc | hd :: tl -> loop ([ hd ] :: acc) tl
  in

  loop [] lst

let pick_two lst =
  let rec loop lst =
    match lst with 
    | a::b::[] 

(* let get_neighbour_states state =
   let stuff_on_this_floor = List.nth_exn state.floors state.elevator_floor in
   let states_with_one_pick = List.concat_map stuff_on_this_floor ~f:(fun obj -> {state with }) *)

(* let solve_p1 floors elevator =

     let rec loop step_count state =
       if is_target state then step_count
       else

   let resultP1 = 0
   let resultP2 = 0 *)
