(* main.ml *)

(* Solution to puzzles from Day 21 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        line :: parse_lines()
    | None ->
        close_in ic;
        []
  in

  parse_lines()


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let testing = false

(* ------------------------------------------------------------------------- *)
(* Part 1. *)

type code = (char * int) list
type sequence = (char * int) list

type position = { x : int; y : int; }

let position x y = { x; y; }

(*
  +---+---+---+
  | 7 | 8 | 9 |
  +---+---+---+
  | 4 | 5 | 6 |
  +---+---+---+
  | 1 | 2 | 3 |
  +---+---+---+
      | 0 | A |
      +---+---+
*)

let get_numeric_pad_position button =
  match button with
  (* 7 | 8 | 9 | *)
  | '7' ->
      position 0 0
  | '8' ->
      position 0 1
  | '9' ->
      position 0 2

  (* 4 | 5 | 6 | *)
  | '4' ->
      position 1 0
  | '5' ->
      position 1 1
  | '6' ->
      position 1 2

  (* 1 | 2 | 3 | *)
  | '1' ->
      position 2 0
  | '2' ->
      position 2 1
  | '3' ->
      position 2 2

  (*   | 0 | A | *)
  | '0' ->
      position 3 1
  | 'A' ->
      position 3 2

  | _ ->
      assert false

(*
      +---+---+
      | ^ | A |
  +---+---+---+
  | < | v | > |
  +---+---+---+
*)

let get_directional_pad_position button =
  match button with
  (*   | ^ | A | *)
  | '^' ->
      position 0 1
  | 'A' ->
      position 0 2

  (* < | v | > | *)
  | '<' ->
      position 1 0
  | 'v' ->
      position 1 1
  | '>' ->
      position 1 2

  | _ ->
      assert false

type pad = Numeric | Directional

let get_position pad button =
  match pad with
  | Numeric ->
      get_numeric_pad_position button
  | Directional ->
      get_directional_pad_position button

let distance pos_a pos_b =
  { x = pos_b.x - pos_a.x; y = pos_b.y - pos_a.y; }

let paths pad button_a button_b =
  let vertical dx = if dx < 0 then ('^', Int.abs dx) else ('v', dx) in
  let horizontal dy = if dy < 0 then ('<', Int.abs dy) else ('>', dy) in
  let { x = dx; y = dy; } = distance button_a button_b in
  if dx <> 0 && dy <> 0 then begin
    if pad = Numeric && button_a.x = 3 && button_a.y + dy = 0 then
      `One [vertical dx; horizontal dy]
    else if pad = Numeric && button_a.x + dx = 3 && button_a.y = 0 then
      `One [horizontal dy; vertical dx]

    else if pad = Directional && button_a.x + dx = 0 && button_a.y = 0 then
      `One [horizontal dy; vertical dx]
    else if pad = Directional && button_a.x = 0 && button_a.y + dy = 0 then
      `One [vertical dx; horizontal dy]

    else
      `Two ([vertical dx; horizontal dy], [horizontal dy; vertical dx])
  end else if dx <> 0 then
    `One [vertical dx]
  else if dy <> 0 then
    `One [horizontal dy]
  else
    `Zero

let get_numeric_char pair =
  match pair with
  | (0, 0) -> '7'
  | (0, 1) -> '8'
  | (0, 2) -> '9'
  | (1, 0) -> '4'
  | (1, 1) -> '5'
  | (1, 2) -> '6'
  | (2, 0) -> '1'
  | (2, 1) -> '2'
  | (2, 2) -> '3'
  | (3, 1) -> '0'
  | (3, 2) -> 'A'
  | (x, y) ->
      Printf.printf "\nFailed in [get_numeric_char] with (%d, %d)\n" x y;
      assert false

let get_directional_char pair =
  match pair with
  | (0, 1) -> '^'
  | (0, 2) -> 'A'
  | (1, 0) -> '<'
  | (1, 1) -> 'v'
  | (1, 2) -> '>'
  | (x, y) ->
      Printf.printf "\nFailed in [get_directional_char] with (%d, %d)\n" x y;
      assert false

let get_char pad { x; y; } =
  match pad with
  | Numeric ->
      get_numeric_char (x, y)
  | Directional ->
      get_directional_char (x, y)

let rec read pads sequence =
  match pads with
  | [] ->
      sequence
  | pad :: pads ->
      let curr_pos = ref (get_position pad 'A') in
      let code = ref [] in
      List.iter (fun (c, times) ->
        let { x; y; } = !curr_pos in
        match c with
        | '^' ->
            curr_pos := position (x - times) y
        | '>' ->
            curr_pos := position x (y + times)
        | 'v' ->
            curr_pos := position (x + times) y
        | '<' ->
            curr_pos := position x (y - times)
        | 'A' ->
            code := !code @ [(get_char pad !curr_pos, times)]
        | c ->
            Printf.printf "\nFailed in [read] with '%c'\n" c;
            assert false
      ) sequence;
      read pads !code

(*let sequence_aux = Utils.memoize_ref (fun sequence_aux (path, times, pads) ->
  match pads with
  | [] ->
      
    match path with
    | `Zero ->
        []
    | `One path ->
        path
    | `Two (path_a, path_b) ->
        if pads = [] then path_a else
          let sequence_a = sequence_aux (path)
  )*)

let rec sequence pads code =
  match pads with
  | [] ->
      code
  | pad :: pads ->
      let out_sequence = ref [] in
      let prev_button = ref 'A' in

      List.iter (fun (next_button, times) ->
        let patch =
          let prev_pos = get_position pad !prev_button in
          let next_pos = get_position pad next_button in
          match paths pad prev_pos next_pos with
          | `Zero ->
              []
          | `One path ->
              sequence pads (path @ [('A', times)])
          | `Two (path_a, path_b) ->
              let sequence_a = sequence pads (path_a @ [('A', times)]) in
              let sequence_b = sequence pads (path_b @ [('A', times)]) in
              if List.(length sequence_a < length sequence_b) then
                sequence_a
              else
                sequence_b
        in

        out_sequence := !out_sequence @ patch;
        prev_button := next_button

      ) code;

      !out_sequence

let to_sequence raw_code =
  let[@tail_mod_cons] rec sequence i c n =
    if i >= String.length raw_code then
      [(c, n)]
    else if raw_code.[i] = c then
       sequence (i + 1) c (n + 1)
    else
       (c, n) :: sequence (i + 1) raw_code.[i] 1
  in
  sequence 1 raw_code.[0] 1

let get_number raw_code =
  Scanf.sscanf raw_code "%dA" (fun number -> number)

let size sequence =
  List.(sum (map snd sequence))

let print_sequence sequence =
  List.iter (fun (c, times) ->
    for _ = 1 to times do Printf.printf "%c" c done
  ) sequence;
  Printf.printf "\n"

let first_puzzle_solution raw_codes =
  let pads = [Numeric; Directional; Directional] in
  let answer = ref 0 in
  List.iter (fun raw_code ->
    let number = get_number raw_code in
    let code = to_sequence raw_code in
    let sequence = sequence pads code in
    let size = size sequence in
(*
    Printf.printf "\n%s -> number = %d && size = %d\n  " raw_code number size;
    print_sequence sequence;

    Printf.printf "  ";
    print_sequence (read [Directional; Directional; Numeric] sequence);
*)
    answer := number * size + !answer
  ) raw_codes;
  !answer

(* ------------------------------------------------------------------------- *)
(* Part 2. *)

module Command = struct
  type t = Up | Down | Left | Right | Press

  let commands = [ Up; Down; Left; Right; Press; ]

  let of_char : char -> t = function
    | '^' ->
        Up
    | 'v' ->
        Down
    | '<' ->
        Left
    | '>' ->
        Right
    | 'A' ->
        Press
    | c ->
        Printf.printf "\nFailed in [Command.of_char] with '%c'\n" c;
        assert false

  let of_string : string -> t list = fun cs ->
    List.init (String.length cs) (fun i -> of_char cs.[i])

  let code = function
    | Up ->
        0
    | Down ->
        1
    | Left ->
        2
    | Right ->
        3
    | Press ->
        4

  let code_to_char i = "^v<>A".[i]

  let of_code i = of_char (code_to_char i)

end

module Digit = struct
  type t = D0 | D1 | D2 | D3 | D4 | D5 | D6 | D7 | D8 | D9 | DA

  let digits = [ D0; D1; D2; D3; D4; D5; D6; D7; D8; D9; DA; ]

  let of_char : char -> t = function
    | '0' ->
        D0
    | '1' ->
        D1
    | '2' ->
        D2
    | '3' ->
        D3
    | '4' ->
        D4
    | '5' ->
        D5
    | '6' ->
        D6
    | '7' ->
        D7
    | '8' ->
        D8
    | '9' ->
        D9
    | 'A' ->
        DA
    | c ->
        Printf.printf "\nFailed in [Digit.of_char] with '%c'\n" c;
        assert false

  let of_string : string -> t list = fun cs ->
    List.init (String.length cs) (fun i -> of_char cs.[i])

  let code = function
    | D0 ->
        0
    | D1 ->
        1
    | D2 ->
        2
    | D3 ->
        3
    | D4 ->
        4
    | D5 ->
        5
    | D6 ->
        6
    | D7 ->
        7
    | D8 ->
        8
    | D9 ->
        9
    | DA ->
        10

  let code_to_char i = "0123456789A".[i]

  let of_code i = of_char (code_to_char i)

  let position = function
    | D0 ->
        position 3 1
    | D1 ->
        position 2 0
    | D2 ->
        position 2 1
    | D3 ->
        position 2 2
    | D4 ->
        position 1 0
    | D5 ->
        position 1 1
    | D6 ->
        position 1 2
    | D7 ->
        position 0 0
    | D8 ->
        position 0 1
    | D9 ->
        position 0 2
    | DA ->
        position 3 2
end


let raw_paths =
  Array.map (Array.map (List.map Command.of_string)) [|
               (* ^ *)        (* v *)         (* < *)         (* > *)         (* A *)
    (* ^ *) [| ["A"];         ["vA"];         ["v<A"];        [">vA"; "v>A"]; [">A"];         |];
    (* v *) [| ["^A"];        ["A"];          ["<A"];         [">A"];         ["^>A"; ">^A"]; |];
    (* < *) [| [">^A"];       [">A"];         ["A"];          [">>A"];        [">>^A"];       |];
    (* > *) [| ["^<A";"<^A"]; ["<A"];         ["<<A"];        ["A"];          ["^A"];         |];
    (* A *) [| ["<A"];        ["<vA"; "v<A"]; ["v<<A"];       ["vA"];         ["A"]           |];
  |]

let paths cmd_a cmd_b = raw_paths.(Command.code cmd_a).(Command.code cmd_b)

let raw_numeric_paths =
  let open Command in
  let raw_numeric_paths = Array.init 11 (fun _ -> Array.make 11 []) in
  let horizontal dy = List.repeat (if dy < 0 then Left else Right) ~times:(Int.abs dy) in
  let vertical dx = List.repeat (if dx < 0 then Up else Down) ~times:(Int.abs dx) in
  for code_a = 0 to 10 do
    let digit_a = Digit.of_code code_a in
    let pos_a = Digit.position digit_a in
    for code_b = 0 to 10 do
      let digit_b = Digit.of_code code_b in
      let pos_b = Digit.position digit_b in
      let { x = dx ; y = dy; } = distance pos_a pos_b in
      if dx = 0 && dy = 0 then
        raw_numeric_paths.(code_a).(code_b) <- [[Press]]
      else if dx = 0 then
        raw_numeric_paths.(code_a).(code_b) <- [horizontal dy @ [Press]]
      else if dy = 0 then
        raw_numeric_paths.(code_a).(code_b) <- [vertical dx @ [Press]]
      else if pos_a.x + dx = 3 && pos_a.y = 0 then
        raw_numeric_paths.(code_a).(code_b) <- [horizontal dy @ vertical dx @ [Press]]
      else if pos_a.x = 3 && pos_a.y + dy = 0 then
        raw_numeric_paths.(code_a).(code_b) <- [vertical dx @ horizontal dy @ [Press]]
      else
        raw_numeric_paths.(code_a).(code_b) <- [
            vertical dx @ horizontal dy @ [Press];
            horizontal dy @ vertical dx @ [Press]
          ]
    done
  done;
  raw_numeric_paths

let () =
  if testing then begin
    for code_a = 0 to 10 do
      let digit_a = Digit.of_code code_a in
      Printf.printf "From '%c'...\n" (Digit.code_to_char code_a);
      for code_b = 0 to 10 do
        let digit_b = Digit.of_code code_b in
        Printf.printf "  to '%c': " (Digit.code_to_char code_b);
        let paths = raw_numeric_paths.(code_a).(code_b) in
        List.iter (fun path ->
          List.iter (fun cmd -> Printf.printf "%c" (Command.(code_to_char (code cmd)))) path;
          Printf.printf " "
        ) paths;
        Printf.printf "\n"
      done
    done
  end

(*let raw_numeric_paths =
  Array.map (Array.map (List.map of_string)) [|
    (* 0 *)  [|
      (* 0 *) ["A"];
      (* 1 *) ["^<A"];
      (* 2 *) ["^A"];
      (* 3 *) ["^>A"];
      (* 4 *) ["^^<A"];
      (* 5 *) ["^^A"];
      (* 6 *) ["^^>A"];
      (* 7 *) ["^^^<A"];
      (* 8 *) ["^^^A"];
      (* 9 *) ["^^^>A"];
      (* A *) [">A"];
    |];

    (* 1 *)  [|
      (* 0 *) [">vA"];
      (* 1 *) ["A"];
      (* 2 *) [">A"];
      (* 3 *) [">>A"];
      (* 4 *) ["^A"];
      (* 5 *) [">^A"; "^>A"];
      (* 6 *) ["^>>A"; ">>^A"];
      (* 7 *) ["^^A"];
      (* 8 *) ["^^>A"; ">^^A"];
      (* 9 *) ["^^>>A"; ">>^^A"];
      (* A *) [">>vA"];
    |];

    (* 2 *)  [|
      (* 0 *) ["vA"];
      (* 1 *) ["<A"];
      (* 2 *) ["A"];
      (* 3 *) [">A"];
      (* 4 *) ["^<A"; "<^A"];
      (* 5 *) ["^<A"; "<^A"];
      (* 6 *) ["^A"];
      (* 7 *) ["^>A"; ">^A"];
      (* 8 *) ["<^A"; "^<A"];
      (* 9 *) ["^^>A"; ">^^A"];
      (* A *) ["v>A"; ">vA"];
    |];

  |]
*)

let path_size head path pairwise_distances =
  let path_len = List.length path in
  let pairs = List.combine (head :: (List.first (path_len - 1) path)) path in
  let distances = List.map (fun (x, y) -> pairwise_distances x y) pairs in
  List.sum distances

let min_path paths_a_b pairwise_distances =
  let paths_lengths =
    List.map (fun path ->
      path_size Command.Press path (fun cmd_a cmd_b ->
        pairwise_distances.(Command.code cmd_a).(Command.code cmd_b)
      )
    ) paths_a_b
  in
  Option.get (List.min ( <= ) paths_lengths)

let lengths n =
  let lengths =
    Array.init n (fun _ -> Array.init 5 (fun _ -> Array.make 5 0))
  in

  List.iter (fun cmd_a ->
    List.iter (fun cmd_b ->
      lengths.(0).(Command.code cmd_a).(Command.code cmd_b) <- List.(length (hd (paths cmd_a cmd_b)))
    ) Command.commands
  ) Command.commands;

  for height = 1 to n - 1 do
    let open List in
    iter (fun cmd_a ->
      iter (fun cmd_b ->
        lengths.(height).(Command.code cmd_a).(Command.code cmd_b) <-
          min_path (paths cmd_a cmd_b) (lengths.(height - 1))
      ) Command.commands
    ) Command.commands
  done;

  lengths
(*fun height cmd_a cmd_b -> lengths.(height).(code cmd_a).(code cmd_b)*)

let raw_numeric_paths_lengths n =
  let lengths_n = lengths n in
  Array.map (Array.map (fun paths_a_b -> min_path paths_a_b lengths_n.(n - 1))) raw_numeric_paths

let print_table table =
  let print_line i =
    Printf.printf "   %c" (Command.code_to_char i);
    for j = 0 to 4 do
      Printf.printf "%4d" table.(i).(j);
    done;
    Printf.printf "\n"
  in

  Printf.printf "       ^   v   <   >   A\n";
  for i = 0 to 4 do
    print_line i
  done

let () =
  if testing then begin
    let lengths_25 = lengths 25 in
    Printf.printf "Level 0:\n";
    print_table lengths_25.(0);

    Printf.printf "\nLevel 1:\n";
    print_table lengths_25.(1)
  end

let () =
  if testing then begin
    let numeric_paths = raw_numeric_paths_lengths 2 in
    let print_line i =
      Printf.printf "   %c" (Digit.code_to_char i);
      for j = 0 to 10 do
        Printf.printf "%4d" numeric_paths.(i).(j);
      done;
      Printf.printf "\n"
    in

    Printf.printf "\n       0   1   2   3   4   5   6   7   8   9   A\n";
    for i = 0 to 10 do
      print_line i
    done
  end

let sequence_size raw_code n =
  let code = List.init (String.length raw_code) (fun i -> Digit.of_char raw_code.[i]) in
  let numeric_paths = raw_numeric_paths_lengths n in
  let size = path_size Digit.DA code (fun da db -> numeric_paths.(Digit.code da).(Digit.code db)) in
  size

let second_puzzle_solution raw_codes =
  let answer = ref 0 in
  List.iter (fun raw_code ->
    let size = sequence_size raw_code 25 in
    let number = get_number raw_code in
    let complexity = size * number in
    if testing then Printf.printf "%s: %d %d\n" raw_code number size;
    answer := complexity + !answer
  ) raw_codes;
  !answer


(* ------------------------------------------------------------------------- *)
(* Main. *)

let sample = false

let input = if sample then "day-21/sample.txt" else "day-21/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
