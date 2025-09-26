(* main.ml *)

(* Solution to puzzles from Day 15 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

type warehouse = (char array) array

type move = Up | Left | Down | Right

let interpret_move = function
  | '^' ->
      Up
  | '<' ->
      Left
  | 'v' ->
      Down
  | '>' ->
      Right
  | _ ->
      raise Parsing_error

let rec interpret_moves = function
  | raw_moves :: raw_movess ->
      List.append
        (List.map interpret_move (String.to_list raw_moves))
        (interpret_moves raw_movess)
  | [] ->
      []

let interpret (raw_grid, raw_moves) =
  let grid = List.to_array (List.map String.to_array raw_grid) in
  let moves = interpret_moves raw_moves in
  (grid, moves)

let parse input : warehouse * move list =
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
  |> List.filter ((<>) "")
  |> List.partition (fun line -> line.[0] = '#')
  |> interpret


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let dimensions warehouse =
  (Array.length warehouse, Array.length warehouse.(0))

let find_robot warehouse = with_return { body = fun { return } ->
  let (n, m) = dimensions warehouse in
  for i = 0 to n - 1 do
    for j = 0 to m - 1 do
      if warehouse.(i).(j) = '@' then
        return (i, j)
    done
  done;
  return (-1, -1)
}

type status = Successful | Failed of move

let update_pos (x, y) = function
  | Up ->
      (x - 1, y)
  | Left ->
      (x, y - 1)
  | Down ->
      (x + 1, y)
  | Right ->
      (x, y + 1)

let update ~prev_move_status (warehouse, ((x, y) as robot)) move =
  if prev_move_status = Failed move then (Failed move, robot) else begin
    let ((x', y') as next_pos) = update_pos robot move in
    match warehouse.(x').(y') with
    | 'O' ->
        let i = ref x' and j = ref y' in
        while warehouse.(!i).(!j) = 'O' do
          let i', j' = update_pos (!i, !j) move in
          i := i';
          j := j'
        done;
        if warehouse.(!i).(!j) = '#' then
          (Failed move, robot)
        else begin
          warehouse.(x).(y) <- '.';
          warehouse.(x').(y') <- '@';
          warehouse.(!i).(!j) <- 'O';
          (Successful, next_pos)
        end
    | '#' ->
        (Failed move, robot)
    | '.' ->
        warehouse.(x).(y) <- '.';
        warehouse.(x').(y') <- '@';
        (Successful, next_pos)
    | _ ->
        assert false
  end

let box_gps (x, y) = 100 * x + y

let gps_sum is_box warehouse =
  let sum = ref 0 in
  let (n, m) = dimensions warehouse in
  for x = 0 to n - 1 do
    for y = 0 to m - 1 do
      if is_box x y then
        sum := !sum + box_gps (x, y)
    done
  done;
  !sum

let first_puzzle_solution (warehouse, moves) =
  let _ = List.fold_left (fun (prev_move_status, robot) move ->
      update ~prev_move_status (warehouse, robot) move
    ) (Successful, find_robot warehouse) moves
  in
  let is_box x y = warehouse.(x).(y) = 'O' in
  gps_sum is_box warehouse

let widen warehouse =
  let (n, m) = dimensions warehouse in
  let large_warehouse =
    Array.init n (fun x ->
      Array.init (2 * m) (fun y ->
        match warehouse.(x).(y / 2) with
        | 'O' ->
            if y mod 2 = 0 then '[' else ']'
        | '@' ->
            if y mod 2 = 0 then '@' else '.'
        | ('.' | '#') as c ->
            c
        | _ ->
            assert false
      )
    )
  in
  large_warehouse

type immediate_conflicts = Obstacle | Box of box | Boxes of box * box | Free
and box = int * int

let possibly_conflicting_positions ((x, y) as _box) = function
  | Up ->
      `Two (x - 1, y)
  | Left ->
      `One (x, y - 1)
  | Down ->
      `Two (x + 1, y)
  | Right ->
      `One (x, y + 2)

let immediate_conflicts warehouse box move =
  match possibly_conflicting_positions box move with
  | `Two (x, y) ->
      begin match warehouse.(x).(y), warehouse.(x).(y + 1) with
      | '.', '.' ->
          Free
      | ('#', _) | (_, '#') ->
          Obstacle
      | '[', ']' ->
          Box (x, y)
      | ']', '.' ->
          Box (x, y - 1)
      | '.', '[' ->
          Box (x, y + 1)
      | ']', '[' ->
          Boxes ((x, y - 1), (x, y + 1))
      | _, _ ->
          assert false
      end
  | `One (x, y) ->
      begin match warehouse.(x).(y) with
      | '.' ->
          Free
      | '#' ->
          Obstacle
      | ']' ->
          Box (x, y - 1)
      | '[' ->
          Box (x, y)
      | _ ->
          assert false
      end

type conflicts = Stuck | CanMove of box list

let conflicts warehouse box move =
  let rec conflicts prev_boxes next_boxes =
    match next_boxes with
    | [] ->
        CanMove prev_boxes
    | box :: next_boxes ->
        begin match immediate_conflicts warehouse box move with
        | Free ->
            conflicts (box :: prev_boxes) next_boxes
        | Box box' ->
            conflicts (box :: prev_boxes) (box' :: next_boxes)
        | Boxes (box_l, box_r) ->
            conflicts (box :: prev_boxes) (box_l :: box_r :: next_boxes)
        | Obstacle ->
            Stuck
        end
  in
  conflicts [] [box]

let update_boxes warehouse boxes move =
  (* Erase boxes. *)
  List.iter (fun ((x, y) as _box) ->
    warehouse.(x).(y) <- '.';
    warehouse.(x).(y + 1) <- '.'
  ) boxes;

  (* Compute the new boxes positions. *)
  let boxes = List.map (fun box -> update_pos box move) boxes in

  (* Place boxes. *)
  List.iter (fun ((x, y) as _box) ->
    warehouse.(x).(y) <- '[';
    warehouse.(x).(y + 1) <- ']'
  ) boxes


let update ~prev_move_status (warehouse, ((x, y) as robot)) move =
  if prev_move_status = Failed move then (Failed move, robot) else begin
    let ((x', y') as next_pos) = update_pos robot move in
    match warehouse.(x').(y') with
    | ('[' | ']') ->
        let box = if warehouse.(x').(y') = '[' then (x', y') else (x', y' - 1) in
        begin match conflicts warehouse box move with
          | Stuck ->
              (Failed move, robot)
          | CanMove boxes ->
              update_boxes warehouse boxes move;
              warehouse.(x).(y) <- '.';
              warehouse.(x').(y') <- '@';
              (Successful, next_pos)
        end
    | '#' ->
        (Failed move, robot)
    | '.' ->
        warehouse.(x).(y) <- '.';
        warehouse.(x').(y') <- '@';
        (Successful, next_pos)
    | _ ->
        assert false
  end

let second_puzzle_solution (warehouse, moves) =
  let robot = find_robot warehouse in
  let _ = List.fold_left (fun (prev_move_status, robot) move ->
      update ~prev_move_status (warehouse, robot) move
    ) (Successful, robot) moves
  in
  let is_box x y = warehouse.(x).(y) = '[' in
  gps_sum is_box warehouse


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-15/input.txt"

let main() =
  let (warehouse, moves) = parse input in
  let large_warehouse = widen warehouse in

  let first_answer = first_puzzle_solution (warehouse, moves) in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution (large_warehouse, moves) in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()

