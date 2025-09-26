(* main.ml *)

(* Solution to puzzles from Day 8 of the Advent of Code 2024. *)

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

module Direction : sig
  type t = int * int
  include Hashtbl.HashedType with type t := int * int
  val null : int * int
  val normalize : int * int -> int * int
end = struct
  type t = int * int

  let equal = (=)
  let hash (dx, dy) = Int.((hash dx) lxor (hash dy))

  let null = (0, 0)

  let rec gcd a b = if b = 0 then a else gcd b (a mod b)

  (* Normalizes the input into a pair of relatively prime numbers [(dx, dy)]
     such that [dx > 0] or [dx = 0 && dy > 0]. *)
  let normalize (dx, dy) =
    let (dx, dy) =
      if dx < 0 || (dx = 0 && dy < 0) then
        (-dx, -dy)
      else
        (dx, dy)
    in
    let (dx, dy) =
      let d = gcd (Int.abs dx) (Int.abs dy) in
      if d <> 0 then
        (dx / d, dy / d)
      else
        (dx, dy)
    in
    (dx, dy)

end

module Position : sig
  type t = { x: int; y: int }
  include Hashtbl.HashedType with type t := t
  val distance : t -> t -> Direction.t * int
end = struct
  type t = { x: int; y: int }

  let equal = (=)
  let hash {x; y} = Int.((hash x) lxor (hash y))

  let distance src dst =
    let (dx', dy') = (dst.x - src.x, dst.y - src.y) in
    let ((dx, dy) as direction) = Direction.normalize (dx', dy') in
    if dx = 0 && dy = 0 then
      (direction, 0)
    else if dx <> 0 then
      (direction, dx' / dx)
    else
      (direction, dy' / dy)
end

module Antena : sig
  type t
  val init : unit -> t
  val add_position : Position.t -> t -> unit
  val iter : (Position.t -> unit) -> t -> unit
end = struct
  module PositionHashtbl = Hashtbl.Make(Position)

  type t = unit PositionHashtbl.t

  let init() = PositionHashtbl.create 13

  let add_position position antena =
    PositionHashtbl.add antena position ()

  let iter f antena =
    PositionHashtbl.iter (fun position () -> f position) antena
end

module Roof : sig
  type t
  type map = (char Array.t) Array.t
  val init : map -> t
  val dimensions : t -> int * int
  val iter : (Position.t -> unit) -> t -> unit
  val exists : (Antena.t -> bool) -> t -> bool
end = struct
  module CharHashtbl = Hashtbl.Make(Char)

  type t = {
    antenas: Antena.t CharHashtbl.t;
    width: int;
    height: int
  }
  type map = (char Array.t) Array.t

  let dimensions {width; height; _} = (width, height)

  let init map =
    let width = Array.length map and height = Array.length map.(0) in
    let antenas = CharHashtbl.create 13 in
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        if map.(x).(y) <> '.' then begin
          let freq = map.(x).(y) in
          let antena =
            if not (CharHashtbl.mem antenas freq) then
              CharHashtbl.add antenas freq (Antena.init());
            CharHashtbl.find antenas freq
          in
          Antena.add_position Position.{x; y} antena
        end
      done
    done;
    {antenas; height; width}

  let iter f {width; height; _} =
    for x = 0 to width - 1 do
      for y = 0 to height - 1 do
        f Position.{x; y}
      done
    done

  let exists p {antenas; _} =
    let exception Stop in
    try
      CharHashtbl.iter (fun _ antena ->
        if p antena then raise Stop
      ) antenas;
      false
    with Stop ->
      true
end


let is_antinode antena node_pos =
  let exception Stop in
  let module DirectionHashtbl = Hashtbl.Make(Direction) in
  let directions : (int list) DirectionHashtbl.t = DirectionHashtbl.create 13 in
  try
    Antena.iter (fun antena_pos ->
      let (direction, distance) = Position.distance node_pos antena_pos in
      let distances = try DirectionHashtbl.find directions direction with Not_found -> [] in
      let twice_or_half d d' = d = 2 * d' || 2 * d = d' in
      let distance = Int.abs distance in
      if List.exists (twice_or_half distance) distances then raise Stop;
      DirectionHashtbl.replace directions direction (distance :: distances)
    ) antena;
    false
  with Stop ->
    true

let first_puzzle_solution roof =
  let antinodes = ref 0 in
  Roof.iter (fun node_pos ->
    if Roof.exists (fun antena -> is_antinode antena node_pos) roof then
      incr antinodes
  ) roof;
  !antinodes


let is_harmonic_antinode antena node_pos =
  let exception Stop in
  let module DirectionHashtbl = Hashtbl.Make(Direction) in
  let directions : unit DirectionHashtbl.t = DirectionHashtbl.create 13 in
  try
    Antena.iter (fun antena_pos ->
      let (direction, _) = Position.distance node_pos antena_pos in

      (* direction \in directions ->
           node_pos is in the direction of two antenas of the same frequency *)
      if DirectionHashtbl.mem directions direction then
        raise Stop;

      (* direction = (0, 0) && |directions| <> 0 ->
           node_post is an antena and at least one other antena has the same frequency *)
      if Direction.(equal direction null) && DirectionHashtbl.length directions <> 0 then
        raise Stop;

      (* direction <> (0, 0) && (0, 0) \in directions ->
           node_post is an antena and at least one other antena has the same frequency *)
      if DirectionHashtbl.mem directions Direction.null then
        raise Stop;

      DirectionHashtbl.add directions direction ()
    ) antena;
    false
  with Stop ->
    true

let second_puzzle_solution roof =
  let harmonic_antinodes = ref 0 in
  Roof.iter (fun node_pos ->
    if Roof.exists (fun antena -> is_harmonic_antinode antena node_pos) roof then
      incr harmonic_antinodes
  ) roof;
  !harmonic_antinodes


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-8/input.txt"

let main() =
  let map = parse input in
  let roof = Roof.init map in
  let first_answer = first_puzzle_solution roof in
  let second_answer = second_puzzle_solution roof in
  Printf.printf "Answer #1: %d\n" first_answer;
  Printf.printf "Answer #2: %d\n" second_answer

let () = main()

