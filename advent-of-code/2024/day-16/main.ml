(* main.ml *)

(* Solution to puzzles from Day 16 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        String.to_array line :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  List.to_array (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

module Maze = struct
  type orientation = Up | Left | Down | Right
  type position = { x : int; y : int; }

  type status = Blocked | Free

  type t = (status array) array

  type vertice = { position : position; orientation : orientation; }
  type distance = int

  let equal u v = u = v

  let hash_orientation = function Up -> 0 | Left -> 1 | Down -> 2 | Right -> 3

  let hash ({ position = { x; y; }; orientation; }) =
    Int.(hash x lxor y) lxor (hash_orientation orientation)

  let of_grid grid : t =
    let n = Array.length grid and m = Array.length grid.(0) in
    let maze =
      Array.init n (fun x ->
        Array.init m (fun y ->
          if grid.(x).(y) = '#' then Blocked else Free
        )
      )
    in
    maze

  let dimensions maze =
    (Array.length maze, Array.length maze.(0))

  let start_and_ending maze =
    let (n, m) = dimensions maze in
    let start = { x = n - 2; y = 1; } in
    let ending = { x = 1; y = m - 2; } in
    (start, ending)

  let invert = function
    | Up ->
        Down
    | Left ->
        Right
    | Down ->
        Up
    | Right ->
        Left

  let rotation_score orientation1 orientation2 =
    if orientation1 = orientation2 then
      0
    else if orientation1 = invert orientation2 then
      2000
    else
      1000

  let move { x; y; } = function
    | Up ->
        { x = x - 1; y; }
    | Left ->
        { x; y = y - 1; }
    | Down ->
        { x = x + 1; y; }
    | Right ->
        { x; y = y + 1; }

  exception Not_neighbors

  let all_orientations = [ Up; Left; Down; Right ]

  let build_vertices position =
    List.map (fun orientation ->
      { position; orientation }
    ) all_orientations

  let vertices maze =
    let (n, m) = dimensions maze in
    let vertices = ref [] in
    for x = 0 to n - 1 do
      for y = 0 to m - 1 do
        if maze.(x).(y) = Free then
          vertices := List.append (build_vertices { x; y; }) !vertices
      done
    done;
    !vertices

  let neighbors maze u =
    let ({ x; y; } as position) = move u.position u.orientation in
    let neighbors = if maze.(x).(y) = Blocked then ref [] else ref [({ position; orientation = u.orientation}, 1)] in
    List.iter (fun orientation ->
      if orientation <> u.orientation then
        let neighbor = ({ position = u.position; orientation; }, rotation_score u.orientation orientation) in
        neighbors := neighbor :: !neighbors
    ) all_orientations;
    !neighbors

end

let first_puzzle_solution grid =
  let module D = Dijkstra(Maze) in
  let maze = Maze.of_grid grid in
  let (start, ending) = Maze.start_and_ending maze in
  let _S = Maze.{ position = start; orientation = Right; } in
  Option.get (D.distance_to_first maze _S (fun Maze.{ position; _; } ->
    position = ending
  ))

module PositionSet =
  Set.Make(struct
    type t = Maze.position
    let compare = Stdlib.compare
  end)

let positions_in_paths maze ~origin:(_S, distances) ~destination:(ending_pos, distance) =
  let rec extensions (w, path, length) =
    if w.Maze.position = ending_pos then
      if length = distance then
        PositionSet.add w.Maze.position path
      else
        PositionSet.empty
    else
      List.fold_left (fun positions (neighbor, edge_dist) ->
        let neighbor_dist = Hashtbl.find distances neighbor in
        if length + edge_dist = neighbor_dist && not (PositionSet.mem neighbor.Maze.position path) then
          let positions_from_neighbor =
            extensions (neighbor, (PositionSet.add w.Maze.position path), neighbor_dist)
          in
          PositionSet.union positions_from_neighbor positions
        else
          positions
      ) PositionSet.empty (Maze.neighbors maze w)
  in
  extensions (_S, PositionSet.empty, 0)

let second_puzzle_solution (distance, grid) =
  let module D = Dijkstra(Maze) in
  let maze = Maze.of_grid grid in
  let (start, ending) = Maze.start_and_ending maze in
  let _S = Maze.{ position = start; orientation = Right; } in
  let distances = Hashtbl.of_seq (D.distances maze _S) in
  let origin = (_S, distances) and destination = (ending, distance) in
  PositionSet.cardinal (positions_in_paths maze ~origin ~destination)


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-16/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution (first_answer, input) in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
