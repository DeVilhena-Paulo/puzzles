(* main.ml *)

(* Solution to puzzles from Day 3 of the Advent of Code 2024. *)


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

open Ast

exception Parsing_error

let parse input =
  let ic = open_in input in
  let buffer = Lexing.from_channel ic in

  try
    let commands = Parser.commands Lexer.token buffer in
    close_in ic;
    commands
  with
    | Parser.Error ->
        (* localisation (Lexing.lexeme_start_p buf); *)
        Printf.eprintf "Syntactic error.\n";
        exit 1


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let first_puzzle_solution commands =
  List.fold_left (fun acc command ->
    match command with
    | Mul (a, b) ->
        acc + a * b
    | _ ->
        acc
  ) 0 commands

let second_puzzle_solution commands =
  let answer, _ = List.fold_left (fun (acc, enabled) command ->
      match command with
      | Mul (a, b) ->
          ((if enabled then acc + a * b else acc), enabled)
      | Do ->
          (acc, true)
      | Dont ->
          (acc, false)
    ) (0, true) commands
  in
  answer


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-3/input.txt"

let main() =
  let commands = parse input in
  let first_answer = first_puzzle_solution commands in
  let second_answer = second_puzzle_solution commands in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()
