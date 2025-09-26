(* main.ml *)

(* Solution to puzzles from Day 10 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line =
  Array.map (fun c ->
    int_of_char c - int_of_char '0'
  ) (String.to_array line)

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        let line = parse_line line in
        line :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  List.to_array (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let dimensions map =
  (Array.length map, Array.length map.(0))

let is_trailhead map (x, y) =
  map.(x).(y) = 0

let find_trailheads map =
  let (n, m) = dimensions map in
  let[@tail_mod_cons] rec find_trailheads (x, y) =
    if x = n then [] else
      if y = m then find_trailheads (x + 1, 0) else (* x <> n && y <> m *)
        if is_trailhead map (x, y) then
          (x, y) :: find_trailheads (x, y + 1)
        else
          find_trailheads (x, y + 1)
  in
  find_trailheads (0, 0)

let trailhead_score map position =
  let (n, m) = dimensions map in
  let visited = Array.init n (fun _ -> Array.make m false) in
  let directions = [(1, 0); (0, 1); (-1, 0); (0, -1)] in
  let rec score level positions =
    if level = 9 then List.length positions else begin
      let next_positions = ref [] in
      List.iter (fun (x, y) ->
        List.iter (fun (dx, dy) ->
          let x', y' = x + dx, y + dy in
          if 0 <= x' && x' < n && 0 <= y' && y' < m then
            if not visited.(x').(y') && map.(x').(y') = level + 1 then begin
              visited.(x').(y') <- true;
              next_positions := (x', y') :: !next_positions
            end
        ) directions
      ) positions;
      score (level + 1) !next_positions
    end
  in
  score 0 [position]

let first_puzzle_solution map =
  let score = ref 0 and trailheads = find_trailheads map in
  List.iter (fun position ->
    score := !score + trailhead_score map position
  ) trailheads;
  !score

let trailhead_rating map positions =
  let (n, m) = dimensions map in
  let directions = [(1, 0); (0, 1); (-1, 0); (0, -1)] in
  let rec rating level positions =
    if level = 9 then List.length positions else begin
      let next_positions = ref [] in
      List.iter (fun (x, y) ->
        List.iter (fun (dx, dy) ->
          let x', y' = x + dx, y + dy in
          if 0 <= x' && x' < n && 0 <= y' && y' < m then
            if map.(x').(y') = level + 1 then
              next_positions := (x', y') :: !next_positions
        ) directions
      ) positions;
      rating (level + 1) !next_positions
    end
  in
  rating 0 positions

let second_puzzle_solution map =
  find_trailheads map
  |> trailhead_rating map


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-10/input.txt"

let main() =
  let input = parse input in
  let first_answer = first_puzzle_solution input in
  let second_answer = second_puzzle_solution input in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()

