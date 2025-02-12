open Core

let isExample = false

let filename =
  if isExample then "lib/day22/example.txt" else "lib/day22/input.txt"

let aoc_input = In_channel.read_lines filename
let node_lines = List.sub aoc_input ~pos:2 ~len:(List.length aoc_input - 2)

type node_t = { x : int; y : int; size : int; used : int }

let string_of_node node =
  Printf.sprintf "Node-x%d-y%d s:%d u:%d a:%d" node.x node.y node.size node.used
    (node.size - node.used)

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
                  let a_avail = a_node.size - a_node.used in
                  let p =
                    if a_node.used <= b_avail && a_node.used > 0 then
                      [ (a_node, b_node) ]
                    else []
                  in
                  let p =
                    if b_node.used <= a_avail && b_node.used > 0 then
                      (b_node, a_node) :: p
                    else p
                  in
                  p @ acc)
          in
          loop (pairs @ acc_pairs) tail
  in
  loop [] nodes

let solve_p1 lines =
  let nodes = List.map lines ~f:get_node in
  let pairs = find_pairs nodes in
  List.length pairs

let string_of_xy x y = Printf.sprintf "%d:%d" x y

let node_str node =
  let s1 = string_of_int node.used in
  let s1 = String.pad_left s1 ~len:4 in
  let s2 = string_of_int node.size in
  let s2 = String.pad_right s2 ~len:4 in
  s1 ^ "/" ^ s2

let print_grid node_map x_max y_max =
  for y = 0 to y_max do
    for x = 0 to x_max do
      let key = string_of_xy x y in
      let node = Hashtbl.find_exn node_map key in
      let s = node_str node in
      Printf.printf "%s" s
    done;
    Printf.printf "\n"
  done

let result_p1 = solve_p1 node_lines

let build_grid lines =
  let nodes = List.map lines ~f:get_node in
  let node_map = Hashtbl.create (module String) in
  let x_max = List.fold nodes ~init:0 ~f:(fun m node -> max m node.x) in
  let y_max = List.fold nodes ~init:0 ~f:(fun m node -> max m node.y) in
  List.iter nodes ~f:(fun node ->
      Hashtbl.add_exn node_map ~key:(string_of_xy node.x node.y) ~data:node);
  (node_map, x_max, y_max)

let grid, x_max, y_max = build_grid node_lines

type pos_t = { x : int; y : int }
type move_t = { from_p : pos_t; to_p : pos_t }

let string_of_pos pos = string_of_xy pos.x pos.y

let build_moves () =
  (* Moves the 0 slot all the way to the left *)
  let moves_left =
    List.init 17 ~f:(fun i ->
        { from_p = { x = 16 - i; y = 22 }; to_p = { x = 17 - i; y = 22 } })
  in
  (* Moves the 0 slot up to 0 0  *)
  let moves_up =
    List.init 22 ~f:(fun i ->
        { from_p = { x = 0; y = 21 - i }; to_p = { x = 0; y = 22 - i } })
  in
  (* Moves the 0 slot all the way to the right *)
  let moves_right =
    List.init x_max ~f:(fun i ->
        { from_p = { x = i + 1; y = 0 }; to_p = { x = i; y = 0 } })
  in
  let moves_around xr =
    [
      { from_p = { x = xr + 1; y = 1 }; to_p = { x = xr + 1; y = 0 } };
      { from_p = { x = xr; y = 1 }; to_p = { x = xr + 1; y = 1 } };
      { from_p = { x = xr - 1; y = 1 }; to_p = { x = xr; y = 1 } };
      { from_p = { x = xr - 1; y = 0 }; to_p = { x = xr - 1; y = 1 } };
      { from_p = { x = xr; y = 0 }; to_p = { x = xr - 1; y = 0 } };
    ]
  in
  let moves_for_top_row =
    List.range 1 x_max |> List.rev |> List.concat_map ~f:moves_around
  in
  moves_left @ moves_up @ moves_right @ moves_for_top_row

let do_move move grid =
  let to_node = Hashtbl.find_exn grid (string_of_pos move.to_p) in
  let from_node = Hashtbl.find_exn grid (string_of_pos move.from_p) in
  let avail = to_node.size - to_node.used in
  if avail < from_node.used then failwith "Illegal move. No room"
  else
    let to_node = { to_node with used = to_node.used + from_node.used } in
    let from_node = { from_node with used = 0 } in
    Hashtbl.set grid ~key:(string_of_pos move.to_p) ~data:to_node;
    Hashtbl.set grid ~key:(string_of_pos move.from_p) ~data:from_node;
    ()

(*
   By printing the grid I found out how to move
   the only node that could fit the data in most cells.
   I made the function build_moves and do_move to test the
   list and help me build it. When I finally got the data 
   moved from cell 37,0 to 0,0 I just got the length of the
   list
*)
let moves = build_moves ()
let _ = List.iter moves ~f:(fun move -> do_move move grid)
let result_p2 = List.length moves
