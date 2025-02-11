open Core

let isExample = false

let filename =
  if isExample then "lib/day22/example.txt" else "lib/day22/input.txt"

let aoc_input = In_channel.read_lines filename
let aoc_input = List.sub aoc_input ~pos:2 ~len:(List.length aoc_input - 2)

type node_t = { x : int; y : int; size : int; used : int }

let string_of_node node =
  let s =
    Printf.sprintf "Node-x%d-y%d s:%d u:%d a:%d" node.x node.y node.size
      node.used (node.size - node.used)
  in
  s

let print_node node = Printf.printf "%s\n" (string_of_node node)

let get_node line =
  let x, y, size, used =
    Scanf.sscanf line "/dev/grid/node-x%d-y%d     %dT   %dT    %dT   %d"
      (fun x y s u _ _ -> (x, y, s, u))
  in
  { x; y; size; used }

let find_pairs nodes =
  let rec loop acc_pairs rest_nodes =
    match rest_nodes with
    | [] -> acc_pairs
    | a_node :: tail ->
        if a_node.used = 0 then loop acc_pairs tail
        else
          let pairs =
            List.fold nodes ~init:[] ~f:(fun acc b_node ->
                if a_node.x = b_node.x && a_node.y = b_node.y then acc
                else
                  let b_avail = b_node.size - b_node.used in
                  if a_node.size <= b_avail then (
                    print_node a_node;
                    Printf.printf "  - %s\n" (string_of_node b_node);
                    Out_channel.flush stdout;
                    Printf.printf "accepted\n";
                    Out_channel.flush stdout;
                    let _ = In_channel.input_line In_channel.stdin in
                    (a_node, b_node) :: acc)
                  else acc)
          in
          loop (pairs @ acc_pairs) tail
  in
  loop [] nodes

(* 748 too low *)

let solve_p1 lines =
  let nodes = List.map lines ~f:get_node in
  let pairs = find_pairs nodes in
  List.iter pairs ~f:(fun pair ->
      let a, b = pair in
      let sa = string_of_node a in
      let sb = string_of_node b in
      Printf.printf "%s  --  %s\n" sa sb);
  List.length pairs

let result_p1 = 0
let result_p2 = 0
