(* main.ml *)

(* Solution to puzzles from Day 9 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line =
  Array.map (fun c ->
    int_of_char c - int_of_char '0'
  ) (String.to_array line)

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

let get_blocks disk_map =
  let size = Array.sum disk_map in
  let blocks = Array.make size None in

  let i = ref 0 and offset = ref 0 in
  while !i < Array.length disk_map do
    let block_size = disk_map.(!i) in

    (* file block *)
    if !i mod 2 = 0 then begin
      let id = !i / 2 in
      for j = !offset to !offset + block_size - 1 do
        blocks.(j) <- Some id
      done
    end;

    offset := !offset + block_size;
    incr i
  done;

  blocks

let is_empty blocks i = Option.is_none blocks.(i)

let is_nonempty blocks i = not (is_empty blocks i)

let checksum blocks =
  let sum = ref 0 in
  Array.iteri (fun i id ->
    if Option.is_some id then
      sum := !sum + i * Option.get id
  ) blocks;
  !sum

let first_puzzle_solution disk_map =
  let blocks = get_blocks disk_map in
  let size = Array.length blocks in

  let i = ref 0 and j = ref (size - 1) in
  while !i < !j do
    if is_nonempty blocks !i then incr i else
      if is_empty blocks !j then decr j else begin
        blocks.(!i) <- blocks.(!j);
        blocks.(!j) <- None;
        incr i;
        decr j
      end
  done;

  checksum blocks

let get_nonempty_blocks disk_map =
  let id_max = (Array.length disk_map + 1) / 2 in
  let nonempty_blocks = Array.make id_max (0, 0) in
  let offset = ref 0 in
  for i = 0 to Array.length disk_map - 1 do
    if i mod 2 = 0 then begin
      let id = i / 2 in
      let new_offset = !offset + disk_map.(i) in
      nonempty_blocks.(id) <- (!offset, new_offset)
    end;
    offset := !offset + disk_map.(i)
  done;
  nonempty_blocks

let get_empty_blocks blocks : (int * int) list =
  let n = Array.length blocks in
  let[@tail_mod_cons] rec empty_blocks i =
    if i >= n then [] else
      if is_nonempty blocks i then
        empty_blocks (i + 1)
      else begin
        let j = ref i in while !j < n && is_empty blocks !j do incr j done;
        (i, !j) :: empty_blocks !j
      end
  in
  empty_blocks 0

let move blocks ((ei, ej) as empty_block) ((ni, nj) as nonempty_block) =
  for offset = 0 to (nj - ni) - 1 do
    blocks.(ei + offset) <- blocks.(ni + offset);
    blocks.(ni + offset) <- None
  done

let second_puzzle_solution disk_map =
  let blocks = get_blocks disk_map in
  let nonempty_blocks = get_nonempty_blocks disk_map in

  let id = ref (Array.length nonempty_blocks - 1) in
  while 0 < !id do
    let empty_blocks = get_empty_blocks blocks in
    begin try
      let size (i, j) = j - i in
      let nonempty_block = nonempty_blocks.(!id) in
      let empty_block =
        List.find (fun empty_block ->
          size nonempty_block <= size empty_block &&
          fst empty_block <= fst nonempty_block
        ) empty_blocks
      in
      move blocks empty_block nonempty_block
    with Not_found -> ()
    end;
    decr id
  done;

  checksum blocks


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-9/input.txt"

let main() =
  let input = parse input in
  let first_answer = first_puzzle_solution input in
  let second_answer = second_puzzle_solution input in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()

