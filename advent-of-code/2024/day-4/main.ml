(* main.ml *)

(* Solution to puzzles from Day 3 of the Advent of Code 2024. *)


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse input =
  let ic = open_in input in
  let[@tail_mod_cons] rec read_lines() =
    match In_channel.input_line ic with
    | Some line ->
        let line = Utils.String.to_array line in
        line :: read_lines()
    | None ->
        close_in ic;
        []
  in
  Utils.List.to_array (read_lines())


(* ------------------------------------------------------------------------- *)
(* Utilities. *)

let count_occurrences text word =
  Seq.length (Utils.String.occurrences ~text ~word)

type direction = Horizontal | Vertical | Diagonal | AntiDiagonal

let starting_pos grid = function
  | Diagonal ->
      (Array.length grid - 1, 0)
  | _ ->
      (0, 0)

let update (x, y) = function
  | Horizontal ->
      (x, y + 1)
  | Vertical ->
      (x + 1, y)
  | Diagonal ->
      (x + 1, y + 1)
  | AntiDiagonal ->
      (x + 1, y - 1)

let next_starting_pos grid direction ((x, y) as _pos) =
  let n = Array.length grid and m = Array.length grid.(0) in
  match direction with
  | Horizontal ->
      if x < n - 1 then Some (x + 1, 0) else None
  | Vertical ->
      if y < m - 1 then Some (0, y + 1) else None
  | Diagonal ->
      let ((sx, sy) as _start) = if x < y then (0, y - x) else (x - y, 0) in
      if 0 < sx then
        Some (sx - 1, sy)
      else if sy < m - 1 then
        Some (sx, sy + 1)
      else
        None
  | AntiDiagonal ->
      let ((sx, sy) as _start) =
        if x < m - 1 - y then (0, x + y) else (x - (m - 1 - y), m - 1)
      in
      if sy < m - 1 then
        Some (sx, sy + 1)
      else if sx < n - 1 then
        Some (sx + 1, sy)
      else
        None

let traverse grid direction =
  let n = Array.length grid and m = Array.length grid.(0) in
  let[@tail_mod_cons] rec path ((x, y) as pos) =
    if 0 <= x && x < n && 0 <= y && y < m then
      grid.(x).(y) :: path (update pos direction)
    else
      match next_starting_pos grid direction pos with
      | Some new_start ->
          path new_start
      | None ->
          []
  in
  path (starting_pos grid direction)


let frame grid =
  let n = Array.length grid and m = Array.length grid.(0) in
  let framed_grid =
    Array.init (n + 2) (fun i ->
      Array.init (m + 2) (fun j ->
        if 1 <= i && i <= n && 1 <= j && j <= m then grid.(i - 1).(j - 1) else '*'
      )
    )
  in
  framed_grid


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let list_to_string : char list -> string = fun cs ->
  String.of_seq (List.to_seq cs)

let first_puzzle_solution grid =
  let grid = frame grid in
  let n = Array.length grid and m = Array.length grid.(0) in

  let directions = [Horizontal; Vertical; Diagonal; AntiDiagonal] in
  let paths = List.map (traverse grid) directions in

  let count = ref 0 in
  List.iter (fun path ->
    assert (List.length path = n * m);
    let path = list_to_string path in
    count := !count + count_occurrences path "XMAS";
    count := !count + count_occurrences path "SAMX"
  ) paths;

  !count

let second_puzzle_solution grid =
  let n = Array.length grid and m = Array.length grid.(0) in

  let extract_X (i, j) = list_to_string [
      grid.(i).(j);
      grid.(i).(j + 2);
      grid.(i + 1).(j + 1);
      grid.(i + 2).(j);
      grid.(i + 2).(j + 2)
    ]
  in

  let is_x_mas = function
    | ("SSAMM" | "MSAMS" | "MMASS" | "SMASM" ) -> true
    | _ -> false
  in

  let count = ref 0 in
  for i = 0 to n - 3 do
    for j = 0 to m - 3 do
      if is_x_mas (extract_X (i, j)) then incr count
    done
  done;

  !count


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-4/input.txt"

let main() =
  let grid = parse input in
  let first_answer = first_puzzle_solution grid in
  let second_answer = second_puzzle_solution grid in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let _test_traverse() =
  let grid = Array.init 3 (fun i -> Array.init 4 (fun j -> (i, j))) in
  let path = traverse grid AntiDiagonal in
  Printf.printf "\npath (size = %d):" (List.length path);
  List.iter (fun (i, j) -> Printf.printf " (%d,%d)" i j) path;
  Printf.printf "\n"

let () = main()
