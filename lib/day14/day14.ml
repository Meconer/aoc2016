open Core

let is_example = false
let salt = if is_example then "abc" else "yjdafjpo"

let get_hash number salt =
  let seed = salt ^ string_of_int number in
  Md5.to_hex (Md5.digest_string seed)

let memo = ref (Map.empty (module Int))

let get_hash_p2 seed salt =
  if Map.mem !memo seed then Map.find_exn !memo seed
  else
    let rec loop hash count =
      if count = 2016 then hash
      else
        let n_hash = Md5.to_hex (Md5.digest_string hash) in
        loop n_hash (count + 1)
    in
    let hash = loop (get_hash seed salt) 0 in
    memo := Map.set !memo ~key:seed ~data:hash;
    hash

let get_triplet s =
  let lst = String.to_list s in
  let rec loop lst =
    match lst with
    | [ _; _ ] | _ :: [] | [] -> None
    | a :: b :: c :: tl ->
        if Char.equal a b && Char.equal b c then Some a else loop (b :: c :: tl)
  in
  loop lst

let get_matching_quint s m =
  let lst = String.to_list s in
  let rec loop lst =
    match lst with
    | [ _; _; _; _ ] | [ _; _; _ ] | [ _; _ ] | _ :: [] | [] -> None
    | a :: b :: c :: d :: e :: tl ->
        if
          Char.equal a m && Char.equal a b && Char.equal b c && Char.equal c d
          && Char.equal d e
        then Some a
        else loop (b :: c :: d :: e :: tl)
  in
  loop lst

let found_matching_quint quint start stop salt =
  let rec loop number =
    if number > stop then false
    else
      let hash = get_hash number salt in
      let q_opt = get_matching_quint hash quint in
      match q_opt with None -> loop (number + 1) | Some _ -> true
  in
  loop start

let found_matching_quint_p2 quint start stop salt =
  let rec loop number =
    if number > stop then false
    else
      let hash = get_hash_p2 number salt in
      let q_opt = get_matching_quint hash quint in
      match q_opt with None -> loop (number + 1) | Some _ -> true
  in
  loop start

let is_valid_key key salt =
  let hash = get_hash key salt in
  match get_triplet hash with
  | None -> false
  | Some triplet -> found_matching_quint triplet (key + 1) (key + 1001) salt

let find_valid_keys salt =
  let rec loop acc key =
    if List.length acc = 64 then acc
    else if is_valid_key key salt then loop (key :: acc) (key + 1)
    else loop acc (key + 1)
  in
  loop [] 0

let result_p1 = List.hd_exn (find_valid_keys salt)

let is_valid_key_p2 key salt =
  let hash = get_hash_p2 key salt in
  match get_triplet hash with
  | None -> false
  | Some triplet -> found_matching_quint_p2 triplet (key + 1) (key + 1000) salt

let find_valid_keys_p2 salt =
  let rec loop acc key =
    if List.length acc = 64 then acc
    else if is_valid_key_p2 key salt then
      (* Printf.printf "Found: %d\n" key;
      Out_channel.flush stdout; *)
      loop (key :: acc) (key + 1)
    else loop acc (key + 1)
  in
  loop [] 0

let result_p2 = List.hd_exn (find_valid_keys_p2 salt)
