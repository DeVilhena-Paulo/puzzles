(* main.ml *)

(* Solution to puzzles from Day 6 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let list_to_array xs =
  Array.init (List.length xs) (List.nth xs)

let string_to_array s = s |> String.to_seq |> List.of_seq |> Array.of_list

let parse_line line =
  string_to_array line

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

  list_to_array (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

type direction = Up | Down | Left | Right

let update ((x, y) as _position) = function
  | Up ->
      (x - 1, y)
  | Down ->
      (x + 1, y)
  | Left ->
      (x, y - 1)
  | Right ->
      (x, y + 1)

(* Rotate 90 degrees right. *)
let rotate = function
  | Up ->
      Right
  | Down ->
      Left
  | Left ->
      Up
  | Right ->
      Down

let inside grid (x, y) =
  let n = Array.length grid and m = Array.length grid.(0) in
  0 <= x && x < n && 0 <= y && y < m

let obstruction grid (x, y) =
  grid.(x).(y) = '#'

let find_guard grid =
  let exception Return of int * int in
  let n = Array.length grid and m = Array.length grid.(0) in
  try
    for x = 0 to n - 1 do
      for y = 0 to m - 1 do
        if grid.(x).(y) = '^' then
          raise (Return (x, y))
      done
    done;
    assert false
  with Return (x, y) ->
    (x, y)

let first_puzzle_solution grid =
  let distinct_positions = ref 0 in
  let visit =
    let visited = Array.(
      init (length grid) (fun _ ->
        make (length grid.(0)) false
      ))
    in
    let visit (x, y) =
      if not visited.(x).(y) then begin
        incr distinct_positions;
        visited.(x).(y) <- true
      end
    in
    visit
  in

  let rec walk direction position =
    visit position;
    let next_position = update position direction in
    if inside grid next_position then
      if obstruction grid next_position then
        walk (rotate direction) position
      else
        walk direction next_position
  in

  walk Up (find_guard grid);

  !distinct_positions

let second_puzzle_solution grid =
  let falls_in_loop grid position =
    let visited = Array.(
      init (length grid) (fun _ ->
        make (length grid.(0)) []
      ))
    in
    let rec walk direction ((x, y) as position) =
      if List.mem direction visited.(x).(y) then true else begin
        visited.(x).(y) <- direction :: visited.(x).(y);
        let next_position = update position direction in
        if not (inside grid next_position) then false else
          if obstruction grid next_position then
            walk (rotate direction) position
          else
            walk direction next_position
      end
    in
    walk Up position
  in

  let n = Array.length grid and m = Array.length grid.(0) in
  let guard_position = find_guard grid in

  let can_obstruct (x, y) =
    (x, y) <> guard_position && not (obstruction grid (x, y))
  in

  let obstructions = ref 0 in
  for x = 0 to n - 1 do
    for y = 0 to m - 1 do
      if can_obstruct (x, y) then begin
        grid.(x).(y) <- '#';
        if falls_in_loop grid guard_position then
          incr obstructions;
        grid.(x).(y) <- '.'
      end
    done
  done;
  !obstructions

(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-6/input.txt"

let main() =
  let input = parse input in
  let first_answer = first_puzzle_solution input in
  let second_answer = second_puzzle_solution input in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()
