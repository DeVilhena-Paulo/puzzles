(* main.ml *)

(* Solution to puzzles from Day 20 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line = String.to_array line

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        (parse_line line) :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  List.to_array (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

let distance_saved = if sample then 76 else 100

type position = { x : int; y : int; }
type orientation = Up | Left | Down | Right

let is_wall racetrack { x; y; } = racetrack.(x).(y) = '#'
let is_not_wall racetrack pos = not (is_wall racetrack pos)

let position x y = { x; y; }

let dimensions racetrack =
  (Array.length racetrack, Array.length racetrack.(0))

let inside racetrack { x; y; } =
  let (n, m) = dimensions racetrack in
  0 <= x && x < n && 0 <= y && y < m

let find_start_and_end graph =
  let (n, m) = dimensions graph in
  let origin = ref None and destination = ref None in
  for x = 0 to n - 1 do
    for y = 0 to m - 1 do
      if graph.(x).(y) = 'S' then
        origin := Some (position x y);
      if graph.(x).(y) = 'E' then
        destination := Some (position x y);
    done
  done;
  Option.(get !origin, get !destination)

let move { x; y; } = function
  | Up ->
      position (x - 1) y
  | Left ->
      position x (y - 1)
  | Down ->
      position (x + 1) y
  | Right ->
      position x (y + 1)

let raw_neighbors pos =
  let directions = [Up; Left; Down; Right] in
  List.map (move pos) directions

let neighbors racetrack pos =
  if is_wall racetrack pos then
    []
  else
    List.filter (is_not_wall racetrack) (raw_neighbors pos)

let distances racetrack ~origin =
  let (n, m) = dimensions racetrack in
  let visited = Array.(init n (fun _ -> make m false)) in
  let distances = Array.(init n (fun _ -> make m (-1))) in

  let rec bfs dist curr_nodes =
    if not (List.is_empty curr_nodes) then begin
      let next_nodes = ref [] in

      List.iter (fun node ->
        List.iter (fun ({ x; y; } as neighbor) ->
          if not visited.(x).(y) then begin
            visited.(x).(y) <- true;
            distances.(x).(y) <- dist + 1;

            next_nodes := neighbor :: !next_nodes
          end
        ) (neighbors racetrack node)
      ) curr_nodes;

      let dist = dist + 1 and curr_nodes = !next_nodes in
      bfs dist curr_nodes
    end
  in

  visited.(origin.x).(origin.y) <- true;
  distances.(origin.x).(origin.y) <- 0;
  bfs 0 [origin];
  fun { x; y; } -> distances.(x).(y)

let manhattan_distance node_a node_b =
  Int.abs (node_a.x - node_b.x) + Int.abs (node_a.y - node_b.y)

module PositionSet = Set.Make(struct
  type t = position
  let compare = Stdlib.compare
end)

let positions ~distance ~origin =
  let positions = ref (PositionSet.singleton origin) in
  for _ = 1 to distance do
    positions := PositionSet.fold (fun pos positions ->
      PositionSet.union positions (PositionSet.of_list (raw_neighbors pos))
    ) !positions !positions
  done;
  !positions

let cheats racetrack ~cheat_distance =
  let (_S, _E) = find_start_and_end racetrack in

  let distances_from_S = distances racetrack ~origin:_S in
  let distances_from_E = distances racetrack ~origin:_E in

  let max_distance = distances_from_S _E - distance_saved in

  let cheats = ref 0 in

  let (n, m) = dimensions racetrack in
  let deltas = positions ~distance:cheat_distance ~origin:(position 0 0) in
  for x = 1 to n - 2 do
    for y = 1 to m - 2 do
      let node = position x y in
      let distance_to_node = distances_from_S node in

      if is_not_wall racetrack node && distance_to_node >= 0 then begin
        let node = position x y in
        PositionSet.iter (fun { x = dx; y = dy; } ->
          let neighbor = position (node.x + dx) (node.y + dy) in
  
          if inside racetrack neighbor && is_not_wall racetrack neighbor then begin
            let distance_to_neighbor = distances_from_E neighbor in

            let distance =
              distance_to_node + distance_to_neighbor + manhattan_distance node neighbor
            in

            if is_not_wall racetrack neighbor
              && distance_to_neighbor >= 0
              && distance <= max_distance then
              incr cheats
          end
        ) deltas
      end
    done
  done;

  !cheats

let first_puzzle_solution racetrack =
  cheats racetrack ~cheat_distance:2

let second_puzzle_solution racetrack =
  cheats racetrack ~cheat_distance:20


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-20/sample.txt" else "day-20/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
