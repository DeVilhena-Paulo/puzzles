(* main.ml *)

(* Solution to puzzles from Day 14 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

type coordinates = { i : int; j : int; }

type robot = {
  mutable position : coordinates;
  velocity : coordinates;
}

type bathtroom = {
  robots : robot list;
  mutable second : int;
}

let make_coordinates i j = { i; j; }

let parse_line robot_raw =
  Scanf.sscanf robot_raw "p=%d,%d v=%d,%d" (fun pj pi vj vi ->
    { position = make_coordinates pi pj;
      velocity = make_coordinates vi vj;
    }
  )

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        parse_line line :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  let robots = parse_lines() and second = 0 in
  { robots; second }


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

let input_limit = make_coordinates 103 101
let sample_limit = make_coordinates 7 11

let limit = if sample then sample_limit else input_limit

let seconds = 100

let update_robot ({ position; velocity; } as robot) =
  let ( mod ) a b = ((a mod b) + b) mod b in
  robot.position <- {
    i = (position.i + velocity.i) mod limit.i;
    j = (position.j + velocity.j) mod limit.j;
  }

let update ({ robots; _ } as bathroom) =
  List.iter update_robot robots;
  bathroom.second <- bathroom.second + 1

let rec count ((one, two, three, four) as quadrants) = function
  | [] ->
      quadrants
  | robot :: robots ->
      if robot.position.i = limit.i / 2 || robot.position.j = limit.j / 2 then
        count quadrants robots
      else if robot.position.i < limit.i / 2 then
        if robot.position.j < limit.j / 2 then
          count (one + 1, two, three, four) robots
        else
          count (one, two + 1, three, four) robots
      else
        if robot.position.j < limit.j / 2 then
          count (one, two, three + 1, four) robots
        else
          count (one, two, three, four + 1) robots

let print_robot { position; _ } =
  Printf.printf "i = %d; j = %d\n" position.i position.j

let first_puzzle_solution ({ robots; _ } as bathroom) =
  while bathroom.second < seconds do
    update bathroom
  done;

  let (one, two, three, four) = count (0, 0, 0, 0) robots in
  one * two * three * four

let display robots =
  let grid = Array.init limit.i (fun _ -> Array.make limit.j 0) in
  List.iter (fun { position = { i; j; }; _ } ->
    grid.(i).(j) <- grid.(i).(j) + 1
  ) robots;
  for i = 0 to limit.i - 1 do
    for j = 0 to limit.j - 1 do
      if grid.(i).(j) <> 0 then
        Printf.printf "%!%d" grid.(i).(j)
      else
        Printf.printf "%!."
    done;
    Printf.printf "%!\n";
  done

let concentrated ~frame:(width, height) robots = Utils.with_return { Utils.body = fun { return } ->
    let grid = Array.init limit.i (fun _ -> Array.make limit.j 0) in

    List.iter (fun { position = { i; j; }; _ } ->
      grid.(i).(j) <- grid.(i).(j) + 1
    ) robots;

    for i = 0 to limit.i - 1 - height do
      for j = 0 to limit.j - 1 - width do
        let count = ref 0 in
        for di = 0 to height - 1 do
          for dj = 0 to width - 1 do
            count := !count + grid.(i + di).(j + dj)
          done
        done;
        if !count = width * height then
          return true
      done
    done;

    return false
  }

let second_puzzle_solution ({ robots; _ } as bathroom) =
  let exception Stop in
  try while true do
    while not (concentrated ~frame:(8, 8) robots) do
      update bathroom
    done;

    Printf.printf "%!\nRobots' positions after %d seconds:\n" bathroom.second;
    display robots;

    Printf.printf "%!\nDoes it look like a Chritmas tree?\n";

    let answer = input_line stdin in

    if answer = "y" || answer = "Y" then
      raise Stop
  done with Stop -> ();
  bathroom.second

(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-14/sample.txt" else "day-14/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()

