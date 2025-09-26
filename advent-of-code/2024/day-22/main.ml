(* main.ml *)

(* Solution to puzzles from Day 22 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        int_of_string line :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  parse_lines()


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

(* ------------------------------------------------------------------------- *)
(* Part 1. *)

let mix a b = a lxor b

let prune a = a mod 16777216

let next secret =
  let secret = prune (mix secret (secret * 64)) in
  let secret = prune (mix secret (secret / 32)) in
  let secret = prune (mix secret (secret * 2048)) in
  secret

let first_puzzle_solution secrets =
  List.(sum (map (next ^. 2000) secrets))


(* ------------------------------------------------------------------------- *)
(* Part 2. *)

let[@tail_mod_rec] rec price_list n secret =
  if n = 2000 then [] else (secret mod 10) :: price_list (n + 1) (next secret)

let except_five ~starting_from:n xs =
  assert (List.length xs >= 5 && 0 <= n && n < 5);
  List.(first (length xs - 4) (last (length xs - n) xs))

let[@tail_mod_rec] rec combine_by_five = function
  | (x :: xs, y :: ys, z :: zs, v :: vs, w :: ws) ->
      (x, y, z, v, w) :: combine_by_five (xs, ys, zs, vs, ws)
  | _ ->
      []

let group_by_five xs =
  let xs' = except_five ~starting_from:0 xs
  and ys' = except_five ~starting_from:1 xs
  and zs' = except_five ~starting_from:2 xs
  and vs' = except_five ~starting_from:3 xs
  and ws' = except_five ~starting_from:4 xs in
  combine_by_five (xs', ys', zs', vs', ws')

let second_puzzle_solution secrets =
  let price_lists_by_five = List.map (group_by_five @. (price_list 0)) secrets in

  let diff_lists =
    let compute_diffs (a, b, c, d, e) = ((b - a, c - b, d - c, e - d), e) in
    List.map (List.map compute_diffs) price_lists_by_five
  in

  let max_bananas = ref 0 in
  let sequences : ((int * int * int * int), int) Hashtbl.t = Hashtbl.create 13 in

  List.iter (fun diffs ->
    let seen : ((int * int * int * int), unit) Hashtbl.t = Hashtbl.create 13 in

    List.iter (fun (sequence, bananas) ->
      if not (Hashtbl.mem seen sequence) then begin
        let old_bananas = try Hashtbl.find sequences sequence with Not_found -> 0 in
        let new_bananas = bananas + old_bananas in

        Hashtbl.replace sequences sequence new_bananas;
        Hashtbl.add seen sequence ();

        if !max_bananas < new_bananas then
          max_bananas := new_bananas
      end
    ) diffs;

  ) diff_lists;

  !max_bananas


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-22/sample.txt" else "day-22/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
