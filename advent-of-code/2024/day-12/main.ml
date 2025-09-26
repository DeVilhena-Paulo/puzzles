(* main.ml *)

(* Solution to puzzles from Day 12 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line = String.to_array line

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

  List.to_array (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let dimensions grid =
  (Array.length grid, Array.length grid.(0))

let find_regions grid =
  let (n, m) = dimensions grid in

  let regions = Array.init n (fun _ -> Array.make m (-1)) in
  let current_region = ref 0 in

  let directions = [(1, 0); (0, 1); (-1, 0); (0, -1)] in

  let rec visit positions =
    let next_positions = ref [] in

    List.iter (fun ((x, y) as _position) ->
      List.iter (fun ((dx, dy) as _direction) ->
        let x', y' = x + dx, y + dy in

        if 0 <= x' && x' < n && 0 <= y' && y' < m then
          if regions.(x').(y') = -1 && grid.(x).(y) = grid.(x').(y') then begin
            regions.(x').(y') <- !current_region;
            next_positions := (x', y') :: !next_positions
          end

      ) directions
    ) positions;

    if !next_positions <> [] then
      visit !next_positions
  in

  for x = 0 to n - 1 do
    for y = 0 to m - 1 do
      if regions.(x).(y) = -1 then begin
        regions.(x).(y) <- !current_region;
        visit [(x, y)];
        incr current_region
      end
    done
  done;

  regions, !current_region

let compute_area (regions, n_regions) =
  let area = Array.make n_regions 0 in
  let (n, m) = dimensions regions in

  for x = 0 to n - 1 do
    for y = 0 to m - 1 do
      area.(regions.(x).(y)) <- area.(regions.(x).(y)) + 1
    done
  done;

  area

let frame regions =
  let (n, m) = dimensions regions in
  let framed_regions =
    Array.init (n + 2) (fun i ->
      Array.init (m + 2) (fun j ->
        if 1 <= i && i <= n && 1 <= j && j <= m then
          regions.(i - 1).(j - 1)
        else -1
      )
    )
  in
  framed_regions

let compute_perimeter (regions, n_regions) =
  let perimeter = Array.make n_regions 0 in

  let regions = frame regions in
  let (n, m) = dimensions regions in

  for x = 0 to n - 1 do
    for y = 0 to m - 2 do
      let a = regions.(x).(y) and b = regions.(x).(y + 1) in
      if a <> b then begin
        if a <> -1 then perimeter.(a) <- perimeter.(a) + 1;
        if b <> -1 then perimeter.(b) <- perimeter.(b) + 1;
      end
    done
  done;

  for y = 0 to m - 1 do
    for x = 0 to n - 2 do
      let a = regions.(x).(y) and b = regions.(x + 1).(y) in
      if a <> b then begin
        if a <> -1 then perimeter.(a) <- perimeter.(a) + 1;
        if b <> -1 then perimeter.(b) <- perimeter.(b) + 1;
      end
    done
  done;

  perimeter


let first_puzzle_solution grid =
  let regions = find_regions grid in

  let area = compute_area regions
  and perimeter = compute_perimeter regions in

  Array.map2 ( * ) area perimeter
  |> Array.sum


type side = Line of int | Column of int

let contigous_partitions xs =
  let rec count x = function
    | [] ->
        1
    | y :: ys ->
        if x + 1 <> y then
          1 + count y ys
        else
          count y ys
  in

  let xs = List.sort Int.compare xs in
  match xs with
  | [] ->
      0
  | y :: ys ->
      count y ys

let compute_sides (regions, n_regions) =
  let sides = Array.init n_regions (fun _ -> Hashtbl.create 13) in

  let regions = frame regions in
  let (n, m) = dimensions regions in

  let update a side position =
    let positions =
      try Hashtbl.find sides.(a) side with Not_found -> []
    in
    Hashtbl.replace sides.(a) side (position :: positions)
  in

  for x = 0 to n - 1 do
    for y = 0 to m - 2 do
      let a = regions.(x).(y) and b = regions.(x).(y + 1) in
      if a <> b then begin
        if a <> -1 then update a (Column y) x;
        (* The negation of [y] is a trick to differentiate sides
           on the column but with opposite directions (one pointing
           "inwards" the region and the other "outwards").
         *)
        if b <> -1 then update b (Column (-y)) x
      end
    done
  done;

  for y = 0 to m - 1 do
    for x = 0 to n - 2 do
      let a = regions.(x).(y) and b = regions.(x + 1).(y) in
      if a <> b then begin
        if a <> -1 then update a (Line x) y;
        if b <> -1 then update b (Line (-x)) y
      end
    done
  done;

  Array.map (fun region_sides ->
    Hashtbl.fold (fun _ positions n_sides ->
      n_sides + contigous_partitions positions
    ) region_sides 0
  ) sides


let second_puzzle_solution grid =
  let regions = find_regions grid in

  let area = compute_area regions
  and sides = compute_sides regions in

  Array.map2 ( * ) area sides
  |> Array.sum


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-12/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()

