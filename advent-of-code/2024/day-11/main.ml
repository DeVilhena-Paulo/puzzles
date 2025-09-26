(* main.ml *)

(* Solution to puzzles from Day 11 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line =
  String.split_on_char ' ' line
  |> List.filter ((<>) "")
  |> List.map int_of_string

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

  List.hd (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let[@tail_mod_cons] rec rev_digits x =
  if x = 0 then [] else (x mod 10) :: rev_digits (x / 10)

let rec of_rev_digits = function
  | [] ->
      0
  | d :: ds ->
      d + 10 * of_rev_digits ds

let split x =
  let ds = rev_digits x in
  let n = List.length ds in
  if n mod 2 <> 0 then None else
    let right_stone  = of_rev_digits (List.first (n / 2) ds) in
    let left_stone = of_rev_digits (List.last  (n / 2) ds) in
    Some (left_stone, right_stone)

let rec count_digits x =
  if x < 10 then 1 else 1 + count_digits (x / 10)

let consume_digits n x =
  let rec consume n order prefix suffix =
    if n = 0 then
      (prefix, suffix)
    else
      consume (n - 1) (order * 10) (prefix / 10) (order * (prefix mod 10) + suffix)
  in
  consume n 1 x 0

let split x =
    let n = count_digits x in
    if n mod 2 = 1 then None else
      Some (consume_digits (n / 2) x)

let[@tail_mod_cons] rec blink = function
  | [] ->
      []
  | 0 :: stones ->
      1 :: blink stones
  | stone :: stones ->
      begin match split stone with
      | Some (left_stone, right_stone) ->
          left_stone :: right_stone :: blink stones
      | None ->
          (2024 * stone) :: blink stones
      end

let first_puzzle_solution stones =
  let n = 25 and stones = ref stones in
  for i = 1 to n do
    stones := blink !stones
  done;
  List.length !stones

let blink_stone = Utils.memoize_rec (fun blink_stone (x, n) ->
    if n = 0 then
      1
    else if x = 0 then
      blink_stone (1, n - 1)
    else
      match split x with
      | None ->
          blink_stone (2024 * x, n - 1)
      | Some (l, r) ->
          blink_stone (l, n - 1) + blink_stone (r, n - 1)
  )

let blink stones ~times:n =
  List.map (fun x -> blink_stone (x, n)) stones
  |> List.sum

let second_puzzle_solution stones =
  blink stones ~times:75


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-11/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()

