open Core

let isExample = true

let filename =
  if isExample then "lib/day24/example.txt" else "lib/day24/input.txt"

let aoc_input = In_channel.read_lines filename
let width = String.length (List.hd_exn aoc_input)
let height = List.length aoc_input

type coord = { x : int; y : int }

(* Define the priority module *)
module IntPriority = struct
  type t = int

  let compare = Int.compare
  let equal = Int.equal
  let hash = Hashtbl.hash
end

(* Define the value module *)
module State = struct
  type t = coord

  let compare a b =
    let cmp = Int.compare a.x b.x in
    if cmp = 0 then Int.compare a.y b.y else cmp

  let equal a b = a.x = b.x && a.y = b.y
  let hash = Hashtbl.hash
end

module StatePSQ = Psq.Make (State) (IntPriority)

let idx_of_pos pos = pos.x + (pos.y * width)
let pos_of_idx idx = { x = idx mod width; y = idx / width }

let find_digits line =
  let rec loop acc idx c_list =
    match c_list with
    | [] -> acc
    | c :: tl ->
        if Char.is_digit c then
          loop ((int_of_char c - int_of_char '0', idx) :: acc) (idx + 1) tl
        else loop acc (idx + 1) tl
  in
  loop [] 0 (String.to_list line)

let build_grid lines =
  Array.of_list (String.to_list (List.reduce_exn lines ~f:(fun a b -> a ^ b)))

let grid = build_grid aoc_input

let find_nodes grid =
  Array.foldi grid ~init:[] ~f:(fun idx acc c ->
      if Char.is_digit c then
        let digit = int_of_char c - int_of_char '0' in
        (digit, idx) :: acc
      else acc)

let get_neighbour_states pos =
  let neighbours =
    [
      { pos with x = pos.x - 1 };
      { pos with x = pos.x + 1 };
      { pos with y = pos.y - 1 };
      { pos with y = pos.y + 1 };
    ]
    |> List.filter ~f:(fun p ->
           let c = grid.(idx_of_pos p) in
           not (Char.equal c '#'))
  in
  neighbours

let get_dist start target =
  let queue = StatePSQ.((add start 0) empty) in

  let rec loop queue =
    if StatePSQ.is_empty queue then None
    else
      let popped = StatePSQ.pop queue in
      match popped with
      | None -> failwith "Empty! Wtf?"
      | Some ((pos, dist), queue) ->
          if State.equal pos target then Some dist
          else
            let neighbours = get_neighbour_states pos in
            let q =
              List.fold neighbours ~init:queue ~f:(fun q neighbour ->
                  let new_pos = neighbour in
                  StatePSQ.add new_pos (dist + 1) q)
            in
            loop q
  in

  loop queue

let node_list = find_nodes grid

let build_edges node_list =
  let rec loop acc nodes =
    match nodes with
    | [] -> acc
    | node :: tail ->
        let edges =
          List.fold node_list ~init:[] ~f:(fun acc b_node ->
              let p1 = pos_of_idx (snd node) in
              let p2 = pos_of_idx (snd b_node) in
              if State.equal p1 p2 then acc
              else
                let dist_opt = get_dist p1 p2 in
                Printf.printf "Get dist for %d:%d - %d:%d\n" p1.x p1.y p2.x p2.y;
                Out_channel.flush stdout;
                match dist_opt with
                | Some dist -> (fst b_node, dist) :: acc
                | None -> failwith "Unreachable")
        in
        loop ((fst node, edges) :: acc) tail
  in
  loop [] node_list

let edges =
  build_edges (find_nodes grid)
  |> List.sort ~compare:(fun a b -> Int.compare (fst a) (fst b))

let find_dist e1 e2 =
  let b = fst e2 in
  let a_to_b = snd (List.find_exn (snd e1) ~f:(fun el -> fst el = b)) in
  a_to_b

let solve_p1 edges =
  let rec loop acc edges =
    match edges with
    | [] -> acc
    | hd :: tl ->
        let dists =
          List.fold tl ~init:[] ~f:(fun acc el ->
              find_dist (fst hd) (fst (snd el)) :: acc)
        in
        loop (dists :: acc) tl
  in
  loop [] edges

let result_p1 = 0
let result_p2 = 0
