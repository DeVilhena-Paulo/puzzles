(* main.ml *)

(* Solution to puzzles from Day 5 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse_section1 line =
  line
  |> String.split_on_char '|'
  |> List.map int_of_string
  |> function [a; b] -> (a, b) | _ -> raise Parsing_error

let parse_section2 line =
  line
  |> String.split_on_char ','
  |> List.map int_of_string

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec read_section1() =
    match In_channel.input_line ic with
    | Some line when String.length line > 0 ->
        let line = parse_section1 line in
        line :: read_section1()
    | _ ->
        []
  in

  let[@tail_mod_cons] rec read_section2() =
    match In_channel.input_line ic with
    | Some line ->
        let line = parse_section2 line in
        line :: read_section2()
    | None ->
        close_in ic;
        []
  in

  let section1 = read_section1() in
  let section2 = read_section2() in

  (section1, section2)


(* ------------------------------------------------------------------------- *)
(* Utilites. *)

module Graph : sig
  type t
  type edge = int * int
  val build : edge list -> t
  val is_edge : t -> edge -> bool
end = struct
  type edge = int * int

  module Edge = struct
    type t = int * int
    let compare = Stdlib.compare
  end

  module EdgeSet = Set.Make(Edge)

  type t = EdgeSet.t

  let build edges =
    List.fold_left (fun set edge -> EdgeSet.add edge set) EdgeSet.empty edges

  let is_edge adj edge = EdgeSet.mem edge adj

end

let valid adj update =
  let n = List.length update in
  List.fold_lefti (fun valid i x -> valid &&
    let check y = not (Graph.is_edge adj (y, x)) in
    List.for_all check (List.last (n - i - 1) update)
  ) true update


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let first_puzzle_solution (rules, updates) =
  let adj = Graph.build rules in
  List.fold_left (fun acc update ->
    if not (valid adj update) then acc else begin
      let i = List.length update / 2 in
      acc + List.nth update i
  end) 0 updates

let second_puzzle_solution (rules, updates) =
  let adj = Graph.build rules in

  let can_be_first x ys =
    List.for_all (fun y -> not (Graph.is_edge adj (y, x))) ys
  in

  let find_first update =
    List.fold_lefti (fun acc i x ->
      match acc with Some _ -> acc | None ->
        let ys = List.remove i update in
        if can_be_first x ys then Some (x, ys) else None
    ) None update
  in

  let[@tail_mod_cons] rec order xs =
    match find_first xs with
    | Some (x, ys) ->
        x :: order ys
    | None ->
        []
  in

  List.fold_left (fun acc update ->
    if valid adj update then acc else
      let update = order update in
      let i = List.length update / 2 in
      acc + List.nth update i
  ) 0 updates


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-5/input.txt"

let main() =
  let input = parse input in
  let first_answer = first_puzzle_solution input in
  let second_answer = second_puzzle_solution input in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()
