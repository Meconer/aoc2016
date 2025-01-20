open Core

let isExample = false

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

let count_chars charlist =
  let sorted = List.sort charlist ~compare:Char.compare in
  let rec loop acc curr_count last_char lst =
    match lst with
    | [] -> List.rev ((last_char, curr_count) :: acc)
    | char :: rest ->
        if Char.equal char last_char then
          loop acc (curr_count + 1) last_char rest
        else if Char.equal last_char '-' then loop acc 1 char rest
        else loop ((last_char, curr_count) :: acc) 1 char rest
  in
  loop [] 0 '-' sorted

let calc_checksum_from_name name_parts =
  let name = Option.value_exn (List.reduce name_parts ~f:(fun a b -> a ^ b)) in
  let charlist = String.to_list name in
  let counts = count_chars charlist in
  let sorted_counts =
    List.sort counts ~compare:(fun a b ->
        let cmp = Int.descending (snd a) (snd b) in
        if cmp = 0 then Char.compare (fst a) (fst b) else cmp)
  in
  let first_five =
    List.sub sorted_counts ~pos:0 ~len:5
    |> List.map ~f:fst |> String.of_char_list
  in
  first_five

let real_room_value room =
  let name_parts, sector_id, checksum = room in
  if String.length checksum <> 5 then 0
  else
    let cs_from_name = calc_checksum_from_name name_parts in
    if String.equal cs_from_name checksum then sector_id else 0

let solve_p1 lines =
  let rooms = parse_input lines in
  List.map rooms ~f:(fun room -> real_room_value room)
  |> List.reduce ~f:(fun a b -> a + b)

let resultP1 = Option.value_exn (solve_p1 aoc_input)

let decrypt_name name_parts decr_key =
  let decrypted =
    List.map name_parts ~f:(fun s ->
        let cl = String.to_list s in
        let dcl =
          List.map cl ~f:(fun c ->
              let c_no = int_of_char c - int_of_char 'a' in
              let decr_c_no = c_no + (decr_key mod 26) in
              char_of_int ((decr_c_no mod 26) + int_of_char 'a'))
        in
        String.of_char_list dcl)
  in
  String.concat ~sep:" " decrypted

let decrypt room =
  let name_parts, sector_id, _ = room in
  let decr_name = decrypt_name name_parts sector_id in
  decr_name

let find_room rooms str_to_find =
  let rec loop rooms =
    match rooms with
    | [] -> failwith ("Did not find " ^ str_to_find)
    | room :: rest ->
        if real_room_value room = 0 then loop rest
        else
          let name = decrypt room in
          if String.is_substring name ~substring:str_to_find then
            let _, sec_id, _ = room in
            sec_id
          else loop rest
  in
  loop rooms

let solve_p2 lines =
  let rooms = parse_input lines in
  let sec_id_of_north = find_room rooms "north" in
  sec_id_of_north

let resultP2 = solve_p2 aoc_input
