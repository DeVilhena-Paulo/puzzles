(* main.ml *)

(* Solution to puzzles from Day 18 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type Definitions. *)

type position = { x : int; y : int; }

type memory = (block array) array

and block = Corrupted | Free

type directions = Up | Left | Down | Right


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let make_position x y = { x; y; }

let parse_line line =
  Scanf.sscanf line "%d,%d" (fun y x -> make_position x y)

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

  parse_lines()


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

let sample_limit = { x = 7; y = 7; }
let input_limit = { x = 71; y = 71; }

let limit = if sample then sample_limit else input_limit
let n_bytes = if sample then 12 else 1024


let dimensions memory =
  (Array.length memory, Array.length memory.(0))

let move { x; y; } = function
  | Up ->
      make_position (x - 1) y
  | Left ->
      make_position x (y - 1)
  | Down ->
      make_position (x + 1) y
  | Right ->
      make_position x (y + 1)

let directions = [ Up; Left; Down; Right ]

let inside { x; y; } =
  0 <= x && x < limit.x && 0 <= y && y < limit.y

let bfs ~origin memory =
  let (n, m) = dimensions memory in

  let visited = Array.init n (fun _ -> Array.make m false) in
  let distance = Array.init n (fun _ -> Array.make m (-1)) in

  let rec bfs curr_level curr_nodes =
    if curr_nodes <> [] then begin
      let next_nodes = ref [] in

      List.iter (fun node ->
        List.iter (fun ({ x; y; } as neighbor) ->

          if inside neighbor && memory.(x).(y) = Free && not visited.(x).(y) then begin
            visited.(x).(y) <- true;
            distance.(x).(y) <- curr_level + 1;
            next_nodes := neighbor :: !next_nodes
          end

        ) (List.map (move node) directions)
      ) curr_nodes;

      bfs (curr_level + 1) !next_nodes
    end
  in

  visited.(0).(0) <- true;
  distance.(0).(0) <- 0;

  bfs 0 [origin];

  distance

let build_memory : memory =
  Array.init limit.x (fun _ -> Array.make limit.y Free)

let corrupt memory bytes =
  List.iter (fun { x; y; } ->
    memory.(x).(y) <- Corrupted
  ) bytes

let first_puzzle_solution bytes =
  let bytes = List.first n_bytes bytes in
  let memory = build_memory in
  corrupt memory bytes;
  let distance = bfs ~origin:(make_position 0 0) memory in
  distance.(limit.x - 1).(limit.y - 1)

let no_path memory ~origin ~target =
  let distance = bfs ~origin memory in
  distance.(target.x).(target.y) = -1

let second_puzzle_solution bytes =
  let memory = build_memory in  

  let origin = make_position 0 0 in
  let target = make_position (limit.x - 1) (limit.y - 1) in

  let exception Found of position in
  try
    List.iter (fun ({ x; y; } as byte) ->
      memory.(x).(y) <- Corrupted;
      if no_path memory ~origin ~target then
        raise (Found byte)
    ) bytes;
    assert false
  with Found byte ->
    (* Invert the coordindates back. *)
    { x = byte.y; y = byte.x; }

(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-18/sample.txt" else "day-18/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d,%d\n" second_answer.x second_answer.y

let () = main()
