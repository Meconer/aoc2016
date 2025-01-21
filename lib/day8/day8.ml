open Core

let isExample = false

let filename =
  if isExample then "lib/day8/example.txt" else "lib/day8/input.txt"

let aoc_input = In_channel.read_lines filename
let width, height = if isExample then (7, 3) else (50, 6)
let idx_of_pos c r = c + (r * width)
let grid = Array.create ~len:(width * height) 0

let add_rect line =
  let w, h = Scanf.sscanf line "rect %dx%d" (fun a b -> (a, b)) in
  for c = 0 to w - 1 do
    for r = 0 to h - 1 do
      grid.(idx_of_pos c r) <- 1
    done
  done

let rotate_row line =
  let row_no, count =
    Scanf.sscanf line "rotate row y=%d by %d" (fun a b -> (a, b))
  in
  let new_row = Array.create ~len:width 0 in
  for c = 0 to width - 1 do
    let source_col = (c - count) % width in
    new_row.(c) <- grid.(idx_of_pos source_col row_no)
  done;
  for c = 0 to width - 1 do
    grid.(idx_of_pos c row_no) <- new_row.(c)
  done

let rotate_column line =
  let col_no, count =
    Scanf.sscanf line "rotate column x=%d by %d" (fun a b -> (a, b))
  in
  let new_col = Array.create ~len:height 0 in
  for r = 0 to height - 1 do
    let source_row = (r - count) % height in
    new_col.(r) <- grid.(idx_of_pos col_no source_row)
  done;
  for r = 0 to height - 1 do
    grid.(idx_of_pos col_no r) <- new_col.(r)
  done

let print_grid () =
  for r = 0 to height - 1 do
    for c = 0 to width - 1 do
      let ch = if grid.(idx_of_pos c r) = 1 then '#' else '.' in
      Printf.printf "%c" ch
    done;
    print_endline ""
  done

let solve_p1 lines =
  let rec build_grid lines =
    match lines with
    | [] -> ()
    | line :: rest ->
        Printf.printf "%s\n" line;
        if String.is_prefix line ~prefix:"rect" then add_rect line;
        if String.is_prefix line ~prefix:"rotate row" then rotate_row line;
        if String.is_prefix line ~prefix:"rotate column" then rotate_column line;
        print_grid ();
        build_grid rest
  in
  build_grid lines;
  let pixel_count = Array.count grid ~f:(fun g -> g = 1) in
  pixel_count

let resultP1 = solve_p1 aoc_input

(* Drawing the grid shows the letters below *)
let resultP2 = "EOARGPHYAO"
