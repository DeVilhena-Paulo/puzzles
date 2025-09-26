(* main.ml *)

(* Solution to puzzles from Day 2 of the Advent of Code 2024. *)

open Utils


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse_line line =
  let words = String.split_on_char ' ' line in
  let non_empty_words = List.filter ((<>) "") words in
  try
    List.map int_of_string non_empty_words
  with _ ->
    raise Parsing_error

let parse input_filename =
  let ic = open_in input_filename in
  let rec consume_lines acc =
    try
      let line = input_line ic in
      consume_lines ((parse_line line) :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  consume_lines []


(* ------------------------------------------------------------------------- *)
(* Utilities. *)

let count p xs = List.length (List.filter p xs)


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let safe report =
  let n = List.length report in

  let report_lst = List.last  (Int.max (n - 1) 0) report in
  let report_fst = List.first (Int.max (n - 1) 0) report in

  let diff = List.map2 (-) report_lst report_fst in

  List.for_all (fun d ->  1 <= d && d <=  3) diff ||
  List.for_all (fun d -> -3 <= d && d <= -1) diff

let first_puzzle_solution reports =
  count safe reports

let safe_rlx report =
  let n = List.length report in
  Seq.exists (fun i -> safe (List.remove i report)) (Seq.range 0 n)

let second_puzzle_solution reports =
  count safe_rlx reports


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input_filename = "day-2/input.txt"

let main() =
  let _ = safe [] in
  let reports = parse input_filename in
  let first_answer = first_puzzle_solution reports in
  let second_answer = second_puzzle_solution reports in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()
