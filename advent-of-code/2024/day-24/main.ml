(* main.ml *)

(* Solution to puzzles from Day 24 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type definitions. *)

type wire = char * char * char

type gate = And | Xor | Or

type status = Waiting of gate * wire * wire | Boolean of bool


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let wire w1 w2 w3 = (w1, w2, w3)

let parse_line1 line =
  Scanf.sscanf line "%c%c%c: %d" (fun w1 w2 w3 v ->
    let wire = wire w1 w2 w3 in
    (wire, Boolean (v = 1))
  )

let parse_line2 line =
  let gate = function
    | 'A' -> And
    | 'X' -> Xor
    | 'O' -> Or
    | _ -> assert false
  in
  Scanf.sscanf line "%c%c%c %c%c%c %c%c%c -> %c%c%c" (fun wa1 wa2 wa3 g _ _ wb1 wb2 wb3 wc1 wc2 wc3 ->
    let wire_a = wire wa1 wa2 wa3 in
    let wire_b = wire wb1 wb2 wb3 in
    let wire_c = wire wc1 wc2 wc3 in
    (wire_c, Waiting (gate g, wire_a, wire_b))
  )

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec read_section1() =
    match In_channel.input_line ic with
    | Some line when String.length line > 0 ->
        let line = parse_line1 line in
        line :: read_section1()
    | _ ->
        []
  in

  let[@tail_mod_cons] rec read_section2() =
    match In_channel.input_line ic with
    | Some line ->
        let line = parse_line2 line in
        line :: read_section2()
    | None ->
        close_in ic;
        []
  in

  let section1 = read_section1() in
  let section2 = read_section2() in

  section1 @ section2


(* ------------------------------------------------------------------------- *)
(* Solutions. *)

let sample = false

(* ------------------------------------------------------------------------- *)
(* Part 1. *)

let interpret gate a b =
  match gate with
  | And ->
      a && b
  | Xor ->
      a <> b
  | Or ->
      a || b

let bin_of_list xs =
  let rec consume_digits acc = function
    | [] ->
        acc
    | d :: ds ->
        consume_digits (2 * acc + if d then 1 else 0) ds
  in
  consume_digits 0 xs

let first_puzzle_solution wires =
  let circuit : (wire, status) Hashtbl.t = Hashtbl.create 13 in

  List.iter (fun (wire, status) -> Hashtbl.add circuit wire status) wires;

  let rec get wire =
    match Hashtbl.find circuit wire with
    | Waiting (gate, wire_a, wire_b) ->
        let a = get wire_a in
        let b = get wire_b in
        let c = interpret gate a b in
        Hashtbl.replace circuit wire (Boolean c);
        c
    | Boolean c ->
        c
  in

  let z_wires =
    wires
    |> List.filter (fun ((w1, _, _), _) -> w1 = 'z')
    |> List.map fst
    |> List.sort (fun za zb -> Stdlib.compare zb za)
    |> List.map (fun wire -> get wire)
  in

  bin_of_list z_wires


(* ------------------------------------------------------------------------- *)
(* Part 2. *)

module Wire = struct
  type t = wire
  let compare = Stdlib.compare
  let equal wire_a wire_b = wire_a = wire_b
  let hash (a, b, c) = Char.(hash a lxor hash b lxor hash c)
end

module Circuit = struct
  module WireHashtbl = Hashtbl.Make(Wire)
  module WireMap = Map.Make(Wire)
  module WireSet = Set.Make(Wire)

  type t = {
    gates : (gate * wire * wire) WireHashtbl.t;
    mutable input_x : int;
    mutable input_y : int;
    output_wires : wire List.t;
    wires : WireSet.t;
  }

  let make raw_wires =
    let gates = WireHashtbl.create 13 in
    List.iter (function
      | wire, Boolean _ ->
          ()
      | wire, Waiting (gate, wire_a, wire_b) ->
          WireHashtbl.add gates wire (gate, wire_a, wire_b)
    ) raw_wires;
    let get_wires c =
      raw_wires
      |> List.filter (fun ((w1, _, _), _) -> w1 = c)
      |> List.sort (fun (za, _) (zb, _) -> Stdlib.compare zb za)
    in
    let get_input c =
      get_wires c
      |> List.map snd
      |> List.filter_map (function Boolean b -> Some b | _ -> None)
      |> bin_of_list
    in
    let input_x = get_input 'x' in
    let input_y = get_input 'y' in
    let output_wires = List.map fst (get_wires 'z') in
    let wires = List.fold_left (fun acc -> function
      | _, Boolean _ -> acc
      | wire, Waiting (_, wire_a, wire_b) ->
          WireSet.(acc |> add wire |> add wire_a |> add wire_b)
      ) WireSet.empty raw_wires
    in
    { gates; input_x; input_y; output_wires; wires; }

  let run t =
    let pos a b = 10 * Char.(code a - code '0') + Char.(code b - code '0') in
    let get = memoize_rec (fun get ((w1, w2, w3) as wire) ->
        if w1 = 'x' then
          t.input_x land (1 lsl (pos w2 w3)) <> 0
        else if w1 = 'y' then
          t.input_y land (1 lsl (pos w2 w3)) <> 0
        else
          let (gate, wire_a, wire_b) = WireHashtbl.find t.gates wire in
          interpret gate (get wire_a) (get wire_b)
      )
    in

    bin_of_list (List.map get t.output_wires)

  let dependencies t wires =
    let dependencies = memoize_rec (fun dependencies ((w1, w2, w3) as wire) ->
      if w1 = 'x' || w1 = 'y' then
        WireSet.empty
      else
        let (_, wire_a, wire_b) = WireHashtbl.find t.gates wire in
        let dependencies_a = dependencies wire_a in
        let dependencies_b = dependencies wire_b in
        WireSet.(union (singleton wire) (union dependencies_a dependencies_b))
      )
    in
    List.fold_left (fun acc wire -> WireSet.union acc (dependencies wire)) WireSet.empty wires

  let input_dependencies t wires =
    let dependencies = memoize_rec (fun dependencies ((w1, w2, w3) as wire) ->
      if w1 = 'x' then
        WireSet.(singleton wire, empty)
      else if w1 = 'y' then
        WireSet.(empty, singleton wire)
      else
        let (_, wire_a, wire_b) = WireHashtbl.find t.gates wire in
        let (xdeps_a, ydeps_a) = dependencies wire_a in
        let (xdeps_b, ydeps_b) = dependencies wire_b in
        WireSet.(union xdeps_a xdeps_b, union ydeps_a ydeps_b)
      )
    in
    List.map dependencies wires

  let gate_to_string = function
    | And -> "&&"
    | Or -> "||"
    | Xor -> "^^"

  let wire_to_string ((w1, w2, w3) as wire) =
    Printf.sprintf "%c%c%c" w1 w2 w3

  let to_string t =
    let to_string ((w1, _, _) as wire) =
      if w1 = 'x' || w1 = 'y' then
        wire_to_string wire
      else
        let (gate, wire_a, wire_b) = WireHashtbl.find t.gates wire in
        let swire = wire_to_string wire in
        let swire_a = wire_to_string wire_a in
        let swire_b = wire_to_string wire_b in
        let sgate = gate_to_string gate in
        Printf.sprintf "%s = %s %s %s" swire swire_a sgate swire_b
    in
    String.join ~sep:"\n" (List.map to_string t.output_wires)

  let definition t wire =
    let rec to_string ((w1, _, _) as wire) =
      if w1 = 'x' || w1 = 'y' then
        wire_to_string wire
      else
        let (gate, wire_a, wire_b) = WireHashtbl.find t.gates wire in
        let swire_a = to_string wire_a in
        let swire_b = to_string wire_b in
        let sgate = gate_to_string gate in
        Printf.sprintf "(%s %s %s)" swire_a sgate swire_b
    in
    to_string wire

  let swap circuit wire_a wire_b =
    if WireSet.mem wire_b (dependencies circuit [wire_a])
     || WireSet.mem wire_a (dependencies circuit [wire_b])
    then
      ()
    else begin
      let gate_a = WireHashtbl.find circuit.gates wire_a in
      let gate_b = WireHashtbl.find circuit.gates wire_b in

      WireHashtbl.replace circuit.gates wire_a gate_b;
      WireHashtbl.replace circuit.gates wire_b gate_a
    end

end

let check circuit i =
  let open Circuit in
  List.for_all (fun (x, y) ->
    circuit.input_x <- x * (1 lsl i);
    circuit.input_y <- y * (1 lsl i);
    run circuit = circuit.input_x + circuit.input_y
  ) [(0, 0); (0, 1); (1, 0); (1, 1)]

let check_all circuit =
  let open Circuit in
  List.for_all (check circuit) (List.range 0 45) &&

  let xs = List.init 1000 (fun _ -> Base.Random.int (1 lsl 44)) in
  let ys = List.init 1000 (fun _ -> Base.Random.int (1 lsl 44)) in

  List.for_all (fun x ->
    List.for_all (fun y ->
      circuit.input_x <- x;
      circuit.input_y <- y;
      run circuit = x + y
    ) xs
  ) ys 

let second_puzzle_solution raw_wires =
  let open Circuit in
  let circuit = make raw_wires in

  (*let deps = WireSet.to_list (dependencies circuit circuit.output_wires) in
  Printf.printf "%d\n" (List.length deps);*)

  let exception Swaps of (wire * wire) list in
  let rec find_swapped_gates swaps i =
    if i = 45 then begin
      if check_all circuit then raise (Swaps swaps) else ()
    end else begin
      if check circuit i then
        find_swapped_gates swaps (i + 1)
      else if List.length swaps = 4 then () else begin

        let deps =
          WireSet.(to_list (diff
            (dependencies circuit (List.last (i + 3) circuit.output_wires))
            (dependencies circuit (List.last i circuit.output_wires))))
        in

        List.iter (fun wire_a ->
          List.iter (fun wire_b ->
            swap circuit wire_a wire_b;
            if check circuit i then
              find_swapped_gates ((wire_a, wire_b) :: swaps) (i + 1);
            swap circuit wire_a wire_b
          ) deps
        ) deps
      end
    end
  in
  let swaps = try find_swapped_gates [] 0; [] with Swaps swaps -> swaps in

  let wires = List.flatten @@ List.map (fun (a, b) -> [a; b]) swaps in
  let swires = String.join ~sep:"," (List.map wire_to_string (List.sort Stdlib.compare wires)) in
  Printf.printf "%s\n" swires;

  List.length swaps

let _second_puzzle_solution raw_wires =
  let open Circuit in
  let circuit = make raw_wires in

  let swaps = ref [] in

  let exception Swap of wire * wire in

  for i = 0 to 44 do
    if not (check circuit i) then begin
      let dependencies =
        WireSet.diff
         (dependencies circuit (List.last (i + 2) circuit.output_wires))
         (dependencies circuit (List.last i circuit.output_wires))
      in

      try
        WireSet.iter (fun wire_a ->
          WireSet.iter (fun wire_b ->
            swap circuit wire_a wire_b;
            if check circuit i then
              raise (Swap (wire_a, wire_b));
            swap circuit wire_a wire_b
          ) dependencies
        ) dependencies
      with Swap (wire_a, wire_b) ->
        swaps := (wire_a, wire_b) :: !swaps
    end
  done;

(*
  let scores =
    List.mapi (fun i wire_pair ->
      let new_circuit = make raw_wires in
      List.iteri (fun j (wire_a, wire_b) ->
        if i <> j then swap new_circuit wire_a wire_b
      ) !swaps;
      check_all new_circuit
    ) !swaps
  in

  List.iteri (fun i score ->
    let wires = List.flatten @@ List.map (fun (a, b) -> [a; b]) (List.remove i !swaps) in
    let swires = String.join ~sep:"," (List.map wire_to_string (List.sort Stdlib.compare wires)) in
    Printf.printf "%s: %d\n" swires score
  ) scores;
*)

(*
  Printf.printf "%s\n" (String.join ~sep:" " (List.map string_of_int (List.map (fun i ->
    let new_circuit = make raw_wires in
    List.iteri (fun j (wire_a, wire_b) ->
      if i <> j then swap new_circuit wire_a wire_b
    ) !swaps;
    check_all new_circuit
  ) (List.range 0 5))));*)

(*
  let print swaps =
    List.iter (fun (wire_a, wire_b) ->
      Printf.printf "(%s, %s)\n" (wire_to_string wire_a) (wire_to_string wire_b)
    ) swaps
  in

  let exception Swaps of (wire * wire) list in

  begin try
    for i = 0 to List.length !swaps - 1 do
      let swaps = List.remove i !swaps in
      let new_circuit = make raw_wires in

      List.iter (fun (wire_a, wire_b) ->
        swap new_circuit wire_a wire_b
      ) swaps;

      if check_all new_circuit then
        raise (Swaps swaps);
    done
  with Swaps swaps ->
    print swaps
  end;
*)

  List.length !swaps

(*
  circuit.input_x <- 0;
  circuit.input_y <- 32;
  let wires = circuit.output_wires in
  let deps = dependencies circuit wires in
  WireSet.cardinal deps
*)

(*
  List.iter (fun (wire, (xdeps, ydeps)) ->
    Printf.printf "%s:\n" (wire_to_string wire);
    Printf.printf "  %s\n" (String.join ~sep:" " (List.map wire_to_string (WireSet.to_list xdeps)));
    Printf.printf "  %s\n" (String.join ~sep:" " (List.map wire_to_string (WireSet.to_list ydeps)));
  ) (List.combine circuit.output_wires (input_dependencies circuit circuit.output_wires));
  let z = run circuit in
  Printf.printf "%d + %d = %d (= %d)\n" circuit.input_x circuit.input_y z (circuit.input_x + circuit.input_y);
  Printf.printf "z00 = %s\n" (definition circuit ('z', '0', '0'));
  Printf.printf "z01 = %s\n" (definition circuit ('z', '0', '1'));
  Printf.printf "z02 = %s\n" (definition circuit ('z', '0', '2'));
  Printf.printf "z03 = %s\n" (definition circuit ('z', '0', '3'));
  Printf.printf "z04 = %s\n" (definition circuit ('z', '0', '4'));
  Printf.printf "z05 = %s\n" (definition circuit ('z', '0', '5'));
*)


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-24/sample.txt" else "day-24/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %d\n" second_answer

let () = main()
