(* main.ml *)

(* Solution to puzzles from Day 1 of the Advent of Code 2024. *)


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse_line line =
  let words = String.split_on_char ' ' line in
  let non_empty_words = List.filter ((<>) "") words in
  try
    let (a, b) =
      match non_empty_words with
      | [a; b] ->
          (a, b)
      | _ ->
          raise Parsing_error
    in
    (int_of_string a, int_of_string b)
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
(* Solutions. *)

let first_puzzle_solution locations1 locations2 =
  let locations1 = List.sort Int.compare locations1 in
  let locations2 = List.sort Int.compare locations2 in
  List.fold_left2 (fun acc location1 location2 ->
    let dist = Int.abs (location1 - location2) in
    acc + dist
  ) 0 locations1 locations2

let second_puzzle_solution locations1 locations2 =
  let freq = Hashtbl.create 13 in
  List.iter (fun location2 ->
    if not (Hashtbl.mem freq location2) then
      Hashtbl.add freq location2 1
    else
      let freq_location2 = Hashtbl.find freq location2 in
      Hashtbl.remove freq location2;
      Hashtbl.add freq location2 (freq_location2 + 1)
  ) locations2;

  let similarity = List.fold_left (fun acc location1 ->
      let freq_location1 =
        match Hashtbl.find_opt freq location1 with
        | None ->
            0
        | Some freq_location1 ->
            freq_location1
      in
      acc + freq_location1 * location1
    ) 0 locations1
  in
  similarity


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input_filename = "day-1/input.txt"

let main() =
  let location_lists = parse input_filename in
  let locations1, locations2 = Utils.List.unzip location_lists in
  let first_answer = first_puzzle_solution locations1 locations2 in
  let second_answer = second_puzzle_solution locations1 locations2 in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()
