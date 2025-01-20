open Core

let isExample = true

let filename =
  if isExample then "lib/day4/example.txt" else "lib/day4/input.txt"

let aoc_input = In_channel.read_lines filename

let get_room_data line =
  let parts = String.split ~on:'-' line in
  let last_part = List.last_exn parts in
  (* Find the sector id *)
  let sector_id = Scanf.sscanf last_part "%d" (fun d -> d) in
  (* Find the checksum string *)
  let l = String.length last_part in
  let pos_opt = String.index last_part '[' in
  let pos =
    match pos_opt with None -> failwith "No bracket found" | Some idx -> idx
  in
  let checksum = String.sub last_part ~pos:(pos + 1) ~len:(l - pos - 2) in
  (* Drop the last part from the name list *)
  let parts = List.drop_last_exn parts in
  (parts, sector_id, checksum)

let parse_input lines =
  let rec loop acc lines =
    match lines with
    | [] -> List.rev acc
    | line :: rest ->
        let encr_name, sector_id, checksum = get_room_data line in
        loop ((encr_name, sector_id, checksum) :: acc) rest
  in
  loop [] lines

let calc_checksum_from_name name_parts =
  let name = Option.value_exn (List.reduce name_parts ~f:(fun a b -> a ^ b)) in
  let charlist = String.to_list name in
  let sorted = List.sort charlist ~compare:Char.compare in
  let counts = List.map sorted 

(* let is_real_room room =
   let name_parts, sector_id, checksum = room in
   if String.length checksum <> 5 then false
   else
     let cs_from_name = calc_checksum_from_name name_parts in *)

let resultP1 = 0
let resultP2 = 0
