(* main.ml *)

(* Solution to puzzles from Day 13 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Theory of grids and coordinates. *)

type cartisian = {
  x : int;
  y : int;
}

type machine = {
  buttons : buttons;
  prize : cartisian;
}
and buttons = { _A : cartisian; _B : cartisian; }

let cartisian x y = { x; y }

module Grid : sig
  type t

  (* Constructs a grid given by a pair of vectors
     written in cartisian coordinates. *)
  val from_axes : axis_a:cartisian -> axis_b:cartisian -> t

  (* Finds coordinates of a given point if it falls in the grid.
     Returns [`Combination (a, b)] if the coordinates [a, b] are unique;
     Returns [`Ambiguous abs] if the axes are linearly dependent and every
       pair [a, b] in the sequence [abs] is a possible solution;
     Returns [`Outside] if the point does not fall in the grid. *)
  val coordinates : t -> cartisian ->
    [ `Combination of int * int
    | `Ambiguous of (int * int) Seq.t
    | `Outside
    ]

end = struct

  type t = {
    axis_a : cartisian;
    axis_b : cartisian;
  }

  let from_axes ~axis_a ~axis_b = { axis_a; axis_b }

  let det { x = x1; y = y1 } { x = x2; y = y2 } =
    x1 * y2 - x2 * y1

  let zero = { x = 0; y = 0 }

  let xgcd a b =
    let rec xgcd (x, (a1, b1)) (y, (a2, b2)) =
      if y = 0 then
        (x, (a1, b1))
      else
        let k = x / y in
        xgcd (y, (a2, b2)) (x mod y, (a1 - k * a2, b1 - k * b2))
    in
    xgcd (a, (1, 0)) (b, (0, 1))

  let gcd a b = fst (xgcd a b)

  let inv a ~modulo:_N =
    let d, (x, _) = xgcd a _N in
    if d <> 1 then None else Some x

  (* ax = b mod N *)
  let solve a b ~modulo:_N =
    let d = gcd a _N in
    if b mod d <> 0 then None else
      let a' = a / d and b' = b / d and  _N' = _N / d in
      let _, (inv_a', _) = xgcd a' _N' in
      let ans = (b' * inv_a' mod _N' + _N') mod _N' in
      Some (ans, d, _N')

  let coeff axis ({ x; y } as pos) =
    (* assumed: axis <> zero. *)
    if axis.x <> 0 then
      if x mod axis.x = 0 then Some (x / axis.x) else None
    else
      if y mod axis.y = 0 then Some (y / axis.y) else None

  let coordinates_ld { axis_a; axis_b } ({ x; y } as pos) =
    if axis_a = zero && axis_b = zero then
      if pos = zero then `Combination (0, 0) else `Outside
    else if axis_a = zero then
      coeff axis_b pos |>
      Option.fold ~none:`Outside ~some:(fun k -> `Combination (0, k))
    else if axis_b = zero then
      coeff axis_a pos |>
      Option.fold ~none:`Outside ~some:(fun k -> `Combination (k, 0))
    else
      if axis_a.x = 0 then
        if axis_b.x <> 0 && x mod axis_b.x = 0 then
          `Combination (0, x / axis_b.x)
        else
          `Outside
      else if axis_b.x = 0 then
        if axis_a.x <> 0 && x mod axis_a.x = 0 then
          `Combination (x / axis_a.x, 0)
        else
          `Outside
      else
        let ka = solve axis_a.x x ~modulo:axis_b.x in
        let kb = solve axis_b.x x ~modulo:axis_a.x in
        begin match ka, kb with
          | Some (ka, da, _Na'), Some (kb, db, _Nb') ->
              `Ambiguous begin Utils.with_yield { Utils.body = fun yield ->
                Seq.iter (fun i -> let ka = ka + i * _Na' in
                  Seq.iter (fun j -> let kb = kb + j * _Nb' in
                    let numerator = x - axis_a.x * ka - axis_b.x * kb in
                    let denominator = axis_a.x * axis_b.x in
                    if numerator mod denominator = 0 then
                      let q = numerator / denominator in
                      (* In fact, any pair
                           [ka + axis_b.x * qa, kb + axis_a.x * qb]
                         with [q = qa + qb] is a solution, but we
                         exploit the fact that [qa = 0] minimizes the
                         total cost to avoid adding all solutions. *)
                      yield (ka, kb + axis_a.x * q)
                  ) (Seq.range 0 db)
                ) (Seq.range 0 da)
              } end
          | _, _ ->
              `Outside
        end

  let coordinates_li { axis_a; axis_b } ({ x; y } as pos) =
    let det_ = det axis_a axis_b in
    let proj_a = axis_b.y * x - axis_b.x * y in
    let proj_b = axis_a.x * y - axis_a.y * x in
    if proj_a mod det_ = 0 && proj_b mod det_ = 0 then
      `Combination (proj_a / det_, proj_b / det_)
    else
      `Outside

  let coordinates ({ axis_a; axis_b } as axes) ({ x; y } as pos) =
    if det axis_a axis_b = 0 then
      coordinates_ld axes pos
    else
      coordinates_li axes pos
end


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

exception Parsing_error

let[@tail_mod_cons] rec interpret = function
  | [] ->
      []
  | [_] | [_; _] ->
      raise Parsing_error
  | button_A_raw :: button_B_raw :: prize_raw :: lines ->
      let _A =
        Scanf.sscanf button_A_raw "Button A: X+%d, Y+%d" cartisian
      in
      let _B =
        Scanf.sscanf button_B_raw "Button B: X+%d, Y+%d" cartisian
      in
      let prize =
        Scanf.sscanf prize_raw    "Prize: X=%d, Y= %d"   cartisian
      in
      let buttons = { _A; _B } in
      { buttons; prize } :: interpret lines

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
  |> List.filter ((<>) "")
  |> interpret


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let min_tokens { buttons; prize } =
  let grid = Grid.from_axes ~axis_a:buttons._A ~axis_b:buttons._B in
  match Grid.coordinates grid prize with
  | `Combination (a, b) ->
      if a >= 0 && b >= 0 then 3 * a + b else 0
  | `Ambiguous abs ->
      let abs = Seq.filter (fun (a, b) -> a >= 0 && b >= 0) abs in
      let costs = Seq.map (fun (a, b) -> 3 * a + b) abs in
      begin match costs() with
      | Seq.Nil ->
          0
      | Seq.Cons (cost, costs) ->
          Seq.fold_left Int.min cost costs
      end
  | `Outside ->
      0

let correct ({ prize; _ } as machine) =
  let delta = 10000000000000 in
  { machine with prize = {
      x = prize.x + delta;
      y = prize.y + delta
    }
  }

let first_puzzle_solution machines =
  List.map min_tokens machines
  |> List.filter (fun x -> x < 400)
  |> List.sum

let second_puzzle_solution machines =
  List.map correct machines
  |> List.map min_tokens
  |> List.sum


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = "day-13/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()

