(* main.ml *)

(* Solution to puzzles from Day 7 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse_line line =
  match (String.split_on_char ':' line) with
  | [total_raw; args_raw] -> begin
      try
        let total = int_of_string total_raw in

        let args_str =
          String.split_on_char ' ' args_raw |> List.filter ((<>) "")
        in
        let args = List.map int_of_string args_str in

        (total, args)
      with _ ->
        raise Parsing_error
    end
  | _ ->
      raise Parsing_error

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

  parse_lines()


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let rec can_be_true ~ops (total, args) =
  match args with
  | [] ->
      total = 0
  | [arg] ->
      total = arg
  | (a :: b :: args) ->
      List.exists (fun op -> can_be_true ~ops (total, ((op a b) :: args))) ops

let first_puzzle_solution equations =
  List.filter (can_be_true ~ops:[( + ); ( * )]) equations
  |> List.map fst
  |> List.sum

(* shift digits of [a] to the left by [log10 b]. *)
let rec shift_left a b = if b = 0 then a else shift_left (10 * a) (b / 10)

let (||.) a b = (shift_left a b) + b

let second_puzzle_solution equations =
  List.filter (can_be_true ~ops:[( + ); ( * ); ( ||. )]) equations
  |> List.map fst
  |> List.sum


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-7/input.txt"

let main() =
  let input = parse input in
  let first_answer = first_puzzle_solution input in
  let second_answer = second_puzzle_solution input in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()

