(* main.ml *)

(* Solution to puzzles from Day 23 of the Advent of Code 2024. *)

open Utils

(* ------------------------------------------------------------------------- *)
(* Type definitions. *)

type pc_name = char * char

type connections = (pc_name * pc_name) list

let make_pc_name a b = (a, b)


(* ------------------------------------------------------------------------- *)
(* Parsing. *)

let parse_line line =
  Scanf.sscanf line "%c%c-%c%c" (fun a b c d ->
    (make_pc_name a b, make_pc_name c d)
  )

let parse input =
  let ic = open_in input in

  let[@tail_mod_cons] rec parse_lines() =
    match In_channel.input_line ic with
    | Some line ->
        parse_line line :: parse_lines()
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

module PcName = struct
  type t = pc_name

  let compare (a, b) (c, d) =
    let cmp_a_c = Char.compare a c in
    if cmp_a_c <> 0 then cmp_a_c else Char.compare b d

  let hash (a, b) =
    Char.(hash a lxor hash b)

  let equal (a, b) (c, d) =
    Char.(equal a c && equal b d)
end

module Graph = struct
  module PcNameSet = Set.Make(PcName)
  module PcNameSetSet = Set.Make(PcNameSet)
  module PcNameHashtbl = Hashtbl.Make(PcName)

  type t = {
    vertices : PcNameSet.t;
    edges : PcNameSetSet.t;
    _adjency_map : PcNameSet.t PcNameHashtbl.t
  }

  let add_edge adjency_map (pc_a, pc_b) =
    let neighbors_a =
      try
        PcNameHashtbl.find adjency_map pc_a
      with Not_found ->
        PcNameSet.empty
    in
    PcNameHashtbl.replace adjency_map pc_a (PcNameSet.add pc_b neighbors_a)

  let make_unordered_pair pc_a pc_b =
    let pair = PcNameSet.(empty |> add pc_a |> add pc_b) in
    if PcNameSet.cardinal pair <> 2 then begin
      let a, b = pc_a and c, d = pc_b in
      Printf.printf
        "Failed at [make_unordered_pair] with [pc_a = %c%c] and [pc_b = %c%c].\n"
        a b c d;
      assert false
    end;
    pair

  let make_triangle pc_a pc_b pc_c =
    let triangle = PcNameSet.(empty |> add pc_a |> add pc_b |> add pc_c) in
    assert (PcNameSet.cardinal triangle = 3);
    triangle

  let make raw_edges =
    let _adjency_map = PcNameHashtbl.create 13 in

    List.iter (fun (pc_a, pc_b) ->
        add_edge _adjency_map (pc_a, pc_b);
        add_edge _adjency_map (pc_b, pc_a)
    ) raw_edges;

    let edge_pairs =
      List.map (fun (pc_a, pc_b) -> make_unordered_pair pc_a pc_b) raw_edges
    in

    let vertices = List.fold_left PcNameSet.union PcNameSet.empty edge_pairs in

    let edges =
      List.fold_left (fun edges edge_pair ->
        PcNameSetSet.add edge_pair edges
      ) PcNameSetSet.empty edge_pairs
    in

    { vertices; edges; _adjency_map; }

  let vertices_from_edge edge =
    let u = ref None and v = ref None in
    PcNameSet.iter (fun a ->
      if !u = None then
        u := Some a
      else if !v = None then
        v := Some a
    ) edge;
    (Option.get !u, Option.get !v)

  let find_triangles graph =
    let triangles = ref PcNameSetSet.empty in

    PcNameSetSet.iter (fun edge ->
      let pc_a, pc_b = vertices_from_edge edge in
      PcNameSet.iter (fun pc_c ->
        if pc_a <> pc_c && pc_b <> pc_c then begin
          let pc_ac = make_unordered_pair pc_a pc_c in
          let pc_bc = make_unordered_pair pc_b pc_c in
          if PcNameSetSet.mem pc_ac graph.edges
            && PcNameSetSet.mem pc_bc graph.edges then begin
              let triangle = make_triangle pc_a pc_b pc_c in
              triangles := PcNameSetSet.add triangle !triangles
          end
        end
      ) graph.vertices
    ) graph.edges;

    !triangles

  let largest_complete_subgraphs graph =
    let can_extend vertices ~with_:v =
      PcNameSet.for_all (fun u ->
        let uv = make_unordered_pair u v in
        PcNameSetSet.mem uv graph.edges
      ) vertices
    in

    let rec largest_complete_subgraphs vertices =
      if PcNameSet.is_empty vertices then
        PcNameSetSet.singleton PcNameSet.empty
      else
        let v = PcNameSet.choose vertices in
        let vertices = PcNameSet.remove v vertices in
      
        let complete_subgraphs = largest_complete_subgraphs vertices in

        PcNameSetSet.fold (fun subgraph subgraphs ->
          if can_extend subgraph ~with_:v then
            PcNameSetSet.add (PcNameSet.add v subgraph) subgraphs
          else
            subgraphs
        ) complete_subgraphs complete_subgraphs
    in
    largest_complete_subgraphs graph.vertices

end

let first_puzzle_solution edges =
  let open Graph in
  let graph = make edges in
  let triangles = find_triangles graph in
  let has_pc_with_t = PcNameSet.exists (fun (c, _) -> c = 't') in
  PcNameSetSet.(cardinal (filter has_pc_with_t triangles))


(* ------------------------------------------------------------------------- *)
(* Part 2. *)

let second_puzzle_solution edges =
  let open Graph in
  let graph = make edges in
  let complete_subgraphs =
    List.sort (fun subgraph_a subgraph_b ->
      - (PcNameSet.(cardinal subgraph_a - cardinal subgraph_b))
    ) (PcNameSetSet.to_list (largest_complete_subgraphs graph))
  in
  let subgraph = List.hd complete_subgraphs in
  let pcs =
    PcNameSet.to_list subgraph
    |> List.sort (PcName.compare)
    |> List.map (fun (a, b) -> Printf.sprintf "%c%c" a b)
  in
  String.join ~sep:"," pcs


(* ------------------------------------------------------------------------- *)
(* Main. *)

let input = if sample then "day-23/sample.txt" else "day-23/input.txt"

let main() =
  let input = parse input in

  let first_answer = first_puzzle_solution input in
  Printf.printf "%!Answer #1: %d\n" first_answer;

  let second_answer = second_puzzle_solution input in
  Printf.printf "%!Answer #2: %s\n" second_answer

let () = main()
