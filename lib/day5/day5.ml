open Core

let isExample = false
let door_id = if isExample then "abc" else "uqwqemis"

let get_password door_id =
  let rec loop acc idx =
    if List.length acc = 8 then String.of_list (List.rev acc)
    else
      let s = door_id ^ string_of_int idx in
      let s = Md5.digest_string s in
      let h = Md5.to_hex s in
      if String.is_prefix h ~prefix:"00000" then
        let pw_char = String.get h 5 in
        loop (pw_char :: acc) (idx + 1)
      else loop acc (idx + 1)
  in
  loop [] 0

let resultP1 = get_password door_id

let get_password_p2 door_id =
  let pw_arr = Array.create ~len:8 '_' in

  let rec loop pw_arr idx =
    if not (Array.exists pw_arr ~f:(fun c -> Char.equal c '_')) then
      String.of_array pw_arr
    else
      let s = door_id ^ string_of_int idx in
      let s = Md5.digest_string s in
      let h = Md5.to_hex s in
      if String.is_prefix h ~prefix:"00000" then
        let pw_pos_c = String.get h 5 in
        let pw_pos = int_of_char pw_pos_c - int_of_char '0' in
        if pw_pos >= 0 && pw_pos < 8 then
          let curr_ch = Array.get pw_arr pw_pos in
          if not (Char.equal curr_ch '_') then loop pw_arr (idx + 1)
          else (
            Array.set pw_arr pw_pos (String.get h 6);
            (* let s = String.of_array pw_arr in
               Printf.printf "%s\n" s;
               Out_channel.flush stdout; *)
            loop pw_arr (idx + 1))
        else loop pw_arr (idx + 1)
      else loop pw_arr (idx + 1)
  in
  loop pw_arr 0

let resultP2 = get_password_p2 door_id
