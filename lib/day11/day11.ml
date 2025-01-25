open Core

let isExample = false

type elevator_t = { floor : int; contents : string list }
type state_t = {floors:string list list; elevator: elevator_t}

let elevator = {floor= 0 ; contents = []}

let floors =
  if isExample then [ [ "HYM"; "LIM" ]; [ "HYG" ]; [ "LIG" ]; [] ]
  else
    [
      [ "POG"; "THG"; "THM"; "PRG"; "RUG"; "RUM"; "COG"; "COM" ];
      [ "POM"; "PRM" ];
      [];
      [];
    ]

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
  
let solve_p1 floors elevator =

  let rec loop step_count state =
    if is_target state then step_count
    else

let resultP1 = 0
let resultP2 = 0
