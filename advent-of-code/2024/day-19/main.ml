(* main.ml *)

(* Solution to puzzles from Day 19 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type Definitions. *)


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let interpret = function
  | stripes_raw :: _ :: designs_raw ->
      let stripes =
        String.split_on_char ',' stripes_raw
        |> List.filter ((<>) "")
        |> List.map String.strip
      in
      let designs = designs_raw in
      stripes, designs
  | _ ->
      raise Parsing_error

let parse_line line = line

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

  interpret (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

let is_path graph ~origin ~destination =
  let rec dfs node =
    node <= destination &&
      (node = destination || List.exists dfs graph.(node))
  in
  dfs origin

let create_graph stripes design =
  let graph = Array.make (String.length design) [] in
  List.iter (fun stripe ->
    Seq.iter (fun node_i ->
      let node_j = node_i + (String.length stripe) in
      graph.(node_i) <- node_j :: graph.(node_i)
    ) (String.occurrences ~text:design ~word:stripe)
  ) stripes;
  graph

let possible stripes design =
  let graph = create_graph stripes design in
  is_path graph ~origin:0 ~destination:(String.length design)

let first_puzzle_solution (stripes, designs) =
  List.length (List.filter (possible stripes) designs)

let count_paths graph ~origin ~destination =
  let dfs = Utils.memoize_rec (fun dfs node ->
    if node > destination then
      0
    else if node = destination then
      1
    else
      List.(sum (map dfs graph.(node)))
  )
  in
  dfs origin

let count_towel_arrangements stripes design =
  let graph = create_graph stripes design in
  count_paths graph ~origin:0 ~destination:(String.length design)

let second_puzzle_solution (stripes, designs) =
  List.sum (List.map (count_towel_arrangements stripes) designs)


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-19/sample.txt" else "day-19/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
