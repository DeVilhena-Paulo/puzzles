(* main.ml *)

(* Solution to puzzles from Day 17 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type Definitions. *)

type instruction = Adv | Bxl | Bst | Jnz | Bxc | Out | Bdv | Cdv

type operand = Lit of int | Combo of register

and register = A | B | C

type opcode = int

type machine = {
  registers : registers;
  instructions : code array;
}

and registers = {
  mutable _A : int;
  mutable _B : int;
  mutable _C : int;
  mutable ip : int;
}

and code = int


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let parse_machine = function
  | register_A_raw :: register_B_raw :: register_C_raw :: _ :: instructions_raw :: [] ->
      let _A = Scanf.sscanf register_A_raw "Register A: %d" (fun r -> r) in
      let _B = Scanf.sscanf register_B_raw "Register B: %d" (fun r -> r) in
      let _C = Scanf.sscanf register_C_raw "Register C: %d" (fun r -> r) in

      let instructions =
        Scanf.sscanf instructions_raw "Program: %s" (fun is -> is)
        |> String.split_on_char ','
        |> List.filter ((<>) "")
        |> List.map int_of_string
        |> Array.of_list
      in

      let machine = { registers = { _A; _B; _C; ip = 0 }; instructions } in
      machine
  | _ ->
      raise Parsing_error

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

  parse_machine (parse_lines())


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let decode_opcode : opcode -> instruction = function
  | 0 ->
      Adv
  | 1 ->
      Bxl
  | 2 ->
      Bst
  | 3 ->
      Jnz
  | 4 ->
      Bxc
  | 5 ->
      Out
  | 6 ->
      Bdv
  | 7 ->
      Cdv
  | _ ->
      assert false

let decode_lit_operand i = Lit i

let decode_combo_operand = function
  | i when 0 <= i && i <= 3 ->
      Lit i
  | 4 ->
      Combo A
  | 5 ->
      Combo B
  | 6 ->
      Combo C
  | _ ->
      assert false

let interpret_operand machine = function
  | Lit i ->
      i
  | Combo A ->
      machine.registers._A
  | Combo B ->
      machine.registers._B
  | Combo C ->
      machine.registers._C

open Effect
open Effect.Deep

type _ Effect.t += Write : int -> unit t

let write x = perform (Write x)

let get_operand machine command =
  match command with
  | Adv ->
      decode_combo_operand (machine.instructions.(machine.registers.ip + 1))
  | Bdv ->
      decode_combo_operand (machine.instructions.(machine.registers.ip + 1))
  | Cdv ->
      decode_combo_operand (machine.instructions.(machine.registers.ip + 1))
  | Bxl ->
      decode_lit_operand (machine.instructions.(machine.registers.ip + 1))
  | Bst ->
      decode_combo_operand (machine.instructions.(machine.registers.ip + 1))
  | Jnz ->
      decode_lit_operand (machine.instructions.(machine.registers.ip + 1))
  | Bxc ->
      decode_lit_operand (machine.instructions.(machine.registers.ip + 1))
  | Out ->
      decode_combo_operand (machine.instructions.(machine.registers.ip + 1))

let[@tail_mod_cons] rec run machine =
  let ip = machine.registers.ip in
  if 0 <= ip && ip + 1 < Array.length machine.instructions then
    let command = decode_opcode (machine.instructions.(ip)) in
    let operand = interpret_operand machine (get_operand machine command) in
    begin match command with
    | Adv ->
        machine.registers._A <- machine.registers._A / (1 lsl operand);
        next_instr machine
    | Bdv ->
        machine.registers._B <- machine.registers._A / (1 lsl operand);
        next_instr machine
    | Cdv ->
        machine.registers._C <- machine.registers._A / (1 lsl operand);
        next_instr machine
    | Bxl ->
        machine.registers._B <- machine.registers._B lxor operand;
        next_instr machine
    | Bst ->
        machine.registers._B <- operand mod 8;
        next_instr machine
    | Jnz ->
        if machine.registers._A = 0 then begin
          next_instr machine
        end else begin
          machine.registers.ip <- operand;
          run machine
        end
    | Bxc ->
        machine.registers._B <- machine.registers._B lxor machine.registers._C;
        next_instr machine
    | Out ->
        next_instr machine ~output:[operand mod 8]
    end

and[@tail_mod_cons] next_instr ?(output=[]) machine =
  machine.registers.ip <- machine.registers.ip + 2;
  List.iter write output;
  run machine

let collect_output machine =
  match run machine with
  | effect Write x, k ->
      x :: continue k ()
  | () ->
      []

let format_output outs =
  String.concat "," (List.map string_of_int outs)

let first_puzzle_solution machine =
  collect_output machine
  |> format_output

let is_self_copying ({ instructions; _; } as machine) =
  let i = ref 0 in
  let n = Array.length instructions in
  match run machine with
  | effect Write x, k ->
      if not (!i < n && instructions.(!i) = x) then false else begin
        incr i;
        continue k ()    
      end
  | () ->
      true

let compare_first_output machine ~target:y =
  match run machine with
  | effect Write x, _ ->
      x = y
  | () ->
      false

let reset machine _A =
  machine.registers._A <- _A;
  machine.registers._B <-  0;
  machine.registers._C <-  0;
  machine.registers.ip <-  0

let rec compute_A machine _A_big_end i =
  if i < 0 then
    [_A_big_end]
  else
    let target = machine.instructions.(i) in
    let _A_candidates =
      List.filter_map (fun _A_little_end ->
        let _A = (_A_big_end lsl 3) lor _A_little_end in
        reset machine _A;
        if compare_first_output machine ~target then
          Some (compute_A machine _A (i - 1))
        else
          None
      ) (List.range 0 8)
    in
    List.flatten _A_candidates

let second_puzzle_solution machine =
  let n = Array.length machine.instructions in
  let _A_candidates = compute_A machine 0 (n - 1) in
  let _A = Option.get (List.min (<=) _A_candidates) in
  assert (reset machine _A; is_self_copying machine);
  _A


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-17/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %s\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
