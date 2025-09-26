(* main.ml *)

(* Solution to puzzles from Day 25 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type definitions. *)

module FiveTuple = struct
  type 'a t = 'a * 'a * 'a * 'a * 'a

  let map f (x0, x1, x2, x3, x4) =
    let y0 = f x0 in let y1 = f x1 in
    let y2 = f x2 in let y3 = f x3 in
    let y4 = f x4 in
    (y0, y1, y2, y3, y4)

  let to_list (x0, x1, x2, x3, x4) =
    [x0; x1; x2; x3; x4]

  let of_list = function
    | [x0; x1; x2; x3; x4] ->
        (x0, x1, x2, x3, x4)
    | xs ->
        let xs = List.map string_of_int xs in
        raise (Invalid_argument ("[" ^ String.join ~sep:", " xs ^ "]"))

  let map2 f (x0, x1, x2, x3, x4) (y0, y1, y2, y3, y4)  =
    let z0 = f x0 y0 in let z1 = f x1 y1 in
    let z2 = f x2 y2 in let z3 = f x3 y3 in
    let z4 = f x4 y4 in
    (z0, z1, z2, z3, z4)

  let for_all p (x0, x1, x2, x3, x4) =
    p x0 && p x1 && p x2 && p x3 && p x4

end

type schematic = Key of int FiveTuple.t | Lock of int FiveTuple.t


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec get_blocks acc =
    match In_channel.input_line ic with
    | Some line ->
        if String.length line > 0 then
          get_blocks (line :: acc)
        else
          (List.rev acc) :: get_blocks []
    | None ->
        close_in ic;
        [List.rev acc]
  in

  let parse_block block =
    let lines = List.first 5 (List.last 6 block) in
    let get line i = if line.[i] = '.' then 0 else 1 in
    let sizes =
      FiveTuple.map (fun i ->
        List.sum (List.map (fun line -> get line i) lines)
      ) (0, 1, 2, 3, 4)
    in
    if (List.hd block).[0] = '.' then Key sizes else Lock sizes

  in

  List.map parse_block (get_blocks [])


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

(* ------------------------------------------------------------------------- *)
(* Part 1. *)

let get_keys keys_and_locks =
  List.filter_map (function Key sizes -> Some sizes | _ -> None) keys_and_locks

let get_locks keys_and_locks =
  List.filter_map (function Lock sizes -> Some sizes | _ -> None) keys_and_locks

let first_puzzle_solution keys_and_locks =
  let keys = get_keys keys_and_locks in
  let locks = get_locks keys_and_locks in

  List.sum @@ List.map (fun key ->
    List.sum @@ List.map (fun lock ->
      if FiveTuple.for_all (fun k -> k <= 5) (FiveTuple.map2 (+) key lock) then
        1
      else
        0
    ) locks
  ) keys


(* ------------------------------------------------------------------------- *)
(* Part 2. *)

(* No Part 2! *)


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-25/sample.txt" else "day-25/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer

let () = main()
