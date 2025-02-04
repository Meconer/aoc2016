open Core

let is_example = false
let initial_state = if is_example then "10000" else "10011111011011001"
let fill_length = if is_example then 20 else 272

let fill initial length =
  let rec loop a =
    if List.length a >= length then List.sub a ~pos:0 ~len:length
    else
      let b =
        List.rev a
        |> List.map ~f:(fun el -> if Char.equal el '1' then '0' else '1')
      in

      loop (a @ [ '0' ] @ b)
  in
  let res = loop (String.to_list initial) in
  res

let rec checksum c_list =
  let rec loop acc lst =
    match lst with
    | [] -> List.rev acc
    | _ :: [] -> failwith "Shouldn't happen if list length is even"
    | a :: b :: rest ->
        if Char.equal a b then loop ('1' :: acc) rest
        else loop ('0' :: acc) rest
  in
  if List.length c_list mod 2 <> 0 then c_list
  else
    let res = loop [] c_list in
    checksum res

let result_p1 = String.of_list (checksum (fill initial_state fill_length))
let result_p2 = String.of_list (checksum (fill initial_state 35651584))
