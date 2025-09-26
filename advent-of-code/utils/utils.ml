(* utils.ml *)

(* ------------------------------------------------------------------------- *)
(* Miscellaneous. *)

(* Function composition. *)
let (@.) f g x = f (g x)

(* Self-composition. *)
let (^.) f n = fun x ->
  let y = ref x in
  for _ = 1 to n do y := f !y done;
  !y


(* ------------------------------------------------------------------------- *)
(* Memoizaton. *)

let memoize f =
  let cache = Hashtbl.create 13 in
  fun x ->
    match Hashtbl.find_opt cache x with
    | Some y ->
        y
    | None ->
        let y = f x in
        Hashtbl.add cache x y;
        y

let toy_memoize_rec ff =
  let f = ref (fun _ -> assert false) in
  let g = memoize (fun x -> ff !f x) in
  f := g;
  g

let memoize_rec ff =
  let cache = Hashtbl.create 13 in
  let rec f x =
    match Hashtbl.find_opt cache x with
    | Some y ->
        y
    | None ->
        let y = ff f x in
        Hashtbl.add cache x y;
        y
  in
  f


(* ------------------------------------------------------------------------- *)
(* Control-flow. *)

type 'a iter = { body : ('a -> unit) -> unit }

let with_yield (type a) { body : (a -> unit) -> unit } : a Seq.t = fun () ->
  let open Effect in
  let open Effect.Deep in
  let open struct type _ Effect.t += Yield : a -> unit t end in
  let yield x = perform (Yield x) in
  match body yield with
  | effect Yield x, k ->
      Seq.Cons (x, continue k)
  | () ->
      Seq.Nil

type 'a return = { return : 'b. 'a -> 'b }
type 'a body = { body : 'b. 'a -> 'b }

let with_return (type a) { body : a return -> _ } : a =
  let exception Return of a in
  try body { return = fun y -> raise (Return y) } with Return y -> y


(* -------------------------------------------------------------------------- *)
(* Number theory. *)

let pow x n =
  let result = ref (if n mod 2 = 1 then x else 1) in
  let base = ref x in
  let exp = ref n in
  while !exp > 0 do
    base := !base * !base;
    exp := !exp / 2;
    if !exp mod 2 = 1 then begin
      result := !result * !base
    end
  done;
  !result

let pow x n =
  let rec pow a x n = (* a.x^n *)
    if n = 0 then a else
      pow (a * (if n mod 2 = 1 then x else 1)) (x * x) (n / 2)
  in
  pow 1 x n

let primes ~up_to:n =
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;
  for p = 2 to n do
    if is_prime.(p) then
      for k = 2 to n / p do
        is_prime.(k * p) <- false
      done
  done;
  is_prime

(* Rebinding the notation [x ** n] to integer exponentiation. *)
let ( **. ) x y = x ** y (* float exponentiation *)
let ( ** ) x n = pow x n (* integer exponentiation *)

module ExtInt = struct
  type t = Int of int | PlusInf

  let show = function Int i -> string_of_int i | _ -> "+inf"

  let get_int = function Int i -> i | _ -> assert false

  let compare a b =
    match a, b with
    | Int a, Int b ->
        Int.compare a b
    | Int _, PlusInf ->
        -1
    | PlusInf, Int _ ->
        1
    | PlusInf, PlusInf ->
        0
end


(* -------------------------------------------------------------------------- *)
(* List. *)

module List = struct
  include List

  let repeat x ~times:n = init n (fun _ -> x)

  let first n xs = take n xs

  let last n xs = drop (Int.max 0 (length xs - n)) xs

  let[@tail_mod_cons] rec remove i = function
    | _ :: ys when i = 0 ->
        ys
    | y :: ys ->
        y :: remove (i - 1) ys
    | [] ->
        []

  let fold_lefti f init xs =
    fst (fold_left (fun (acc, i) x -> (f acc i x, i + 1)) (init, 0) xs)

  let fold_righti f init xs =
    let n = length xs in
    fst (fold_right (fun x (acc, i) -> (f acc i x, i - 1)) xs (init, n))

  let enumerate xs =
    let[@tail_mod_cons] rec enumerate i = function
      | [] ->
          []
      | x :: xs ->
          (i, x) :: enumerate (i + 1) xs
    in
    enumerate 0 xs

  let for_alli p xs =
    for_all (fun (i, x) -> p i x) (enumerate xs)

  let existsi p xs =
    exists (fun (i, x) -> p i x) (enumerate xs)

  (* [@tail_mod_cons] does not work with this function.
     I believe this is a missed optimization opportunity
     by the compiler. *)
  let rec unzip xys =
    match xys with
    | [] ->
        ([], [])
    | (x, y) :: xys ->
        let xs, ys = unzip xys in
        (x :: xs, y :: ys)

  let filter_none xs = filter_map (fun i -> i) xs

  let to_array xs =
    Array.init (length xs) (nth xs)

  let sum = fold_left (+) 0

  let range a b = init (b - a) (fun i -> a + i)

  let min cmp_le = function
    | [] ->
        None
    | x :: xs ->
        Some (fold_left (fun x y -> if cmp_le x y then x else y) x xs)

  let max cmp_le = min (fun x y -> cmp_le y x)

end


(* -------------------------------------------------------------------------- *)
(* Seq. *)

module Seq = struct
  include Seq

  let range a b = init (b - a) (fun i -> a + i)

  let enumerate xs =
    let rec enumerate i xs = fun () ->
      match xs() with
      | Seq.Cons (x, xs) ->
          Seq.Cons ((i, x), enumerate (i + 1) xs)
      | Seq.Nil ->
          Seq.Nil
    in
    enumerate 0 xs

end


(* -------------------------------------------------------------------------- *)
(* Array. *)

module Array = struct
  include Array

  let fold_lefti f init xs =
    fst (fold_left (fun (acc, i) x -> (f acc i x, i + 1)) (init, 0) xs)

  let fold_righti f init xs =
    let n = length xs in
    fst (fold_right (fun x (acc, i) -> (f acc i x, i - 1)) xs (init, n))

  let sum = Array.fold_left (+) 0
end


(* -------------------------------------------------------------------------- *)
(* String. *)

module String = struct
  include String

  let substr s ~pos ~len = sub s pos len

  let strip ?chars s =
    let module CharSet = Set.Make(Char) in

    let chars =
      CharSet.of_seq (to_seq (
        Option.fold ~none:" " ~some:(fun cs -> cs) chars
      ))
    in

    let i = ref 0 in
    while (!i < length s && CharSet.mem s.[!i] chars) do incr i done;

    let j = ref (length s - 1) in
    while (0 <= !j && CharSet.mem s.[!j] chars) do decr j done;

    substr s ~pos:(!i) ~len:(!j - !i + 1)

  let join ?(sep="") ss = concat sep ss

  let explode s = List.init (length s) (fun i -> make 1 s.[i])

  let maximal_borders cs =
    let bs = Array.make (length cs) 0 in
    let j = ref 0 in
    for i = 1 to Array.length bs - 1 do
      while 0 < !j && cs.[i] <> cs.[!j] do j := bs.(!j - 1) done;
      j := if cs.[i] = cs.[!j] then !j + 1 else 0;
      bs.(i) <- !j
    done;
    bs

  let occurrences ~text ~word = with_yield { body = fun yield ->
    let borders = maximal_borders (word ^ "#" ^ text) in
    for i = 0 to String.(length text - length word) do
      let j = i + 2 * (String.length word) in
      if borders.(j) = String.length word then
        yield i
    done
  }

  let to_list s = s |> to_seq |> List.of_seq
  let to_array s = s |> to_list |> Array.of_list
end


(* -------------------------------------------------------------------------- *)
(* Hashtbl. *)

module Hashtbl = struct
  include Hashtbl

  let find_pair (type a b) p (tbl : (a, b) Hashtbl.t) =
    let exception Found of a * b in
    try
      iter (fun key value ->
        if p key value then raise (Found (key, value))
      ) tbl;
      None
    with Found (key, value) ->
      Some (key, value)

  let exists p tbl = Option.is_some (find_pair p tbl)
end


(* -------------------------------------------------------------------------- *)
(* PriorityQueue. *)

module PriorityQueue (Elt : Hashtbl.HashedType) (Wgt : Set.OrderedType) : sig
  type t
  type elt = Elt.t
  type weight = Wgt.t

  exception Empty
  exception Duplicate

  val create : capacity:int -> t

  val empty : t -> bool
  val full : t -> bool

  val push : t -> elt -> weight -> unit
  val update : t -> elt -> weight -> unit

  val mem : t -> elt -> bool

  val get_weight : t -> elt -> weight
  val get_weight_opt : t -> elt -> weight option

  val pop : t -> elt * weight
  val pop_opt : t -> (elt * weight) option

end = struct
  type elt = Elt.t
  type weight = Wgt.t

  module EltHashtbl = Hashtbl.Make(Elt)

  type t = {
    mutable size : int;
    mutable capacity : int;
    mutable elements : (elt option) array;
    keys : int EltHashtbl.t;
    weights : weight EltHashtbl.t;
  }

  exception Empty
  exception Duplicate

  let create ~capacity = {
    size = 0;
    capacity = capacity; (* = Array.length elements *)
    elements = Array.make capacity None;
    keys = EltHashtbl.create 13;
    weights = EltHashtbl.create 13;
  }

  let empty { size; _ } = size = 0

  let full { size; capacity; _ } = size = capacity

  let swap { elements; keys; _ } i j =
    let elt_i = Option.get elements.(i) in
    let elt_j = Option.get elements.(j) in

    elements.(i) <- Some elt_j;
    elements.(j) <- Some elt_i;

    EltHashtbl.replace keys elt_i j;
    EltHashtbl.replace keys elt_j i

  let min_weight { size; elements; weights; _ } l r =
    let elt_l_opt = if l < size then elements.(l) else None in
    let elt_r_opt = if r < size then elements.(r) else None in
    match elt_l_opt, elt_r_opt with
    | None, None ->
        None
    | Some elt_l, None ->
        Some l
    | None, Some elt_r ->
        Some r
    | Some elt_l, Some elt_r ->
        let wgt_l = EltHashtbl.find weights elt_l
        and wgt_r = EltHashtbl.find weights elt_r in
        Some (if Wgt.compare wgt_l wgt_r <= 0 then l else r)

  let rec down ({ elements; weights; _ } as queue) i =
    let l = 2 * i + 1 and r = 2 * i + 2 in
    begin match min_weight queue l r with
    | None ->
        ()
    | Some k ->
        if min_weight queue i k <> Some i then begin
          swap queue i k;
          down queue k
        end
    end

  let rec up ({ elements; _ } as queue) i =
    if 0 < i then begin
      let p = (i - 1) / 2 in
      if min_weight queue i p <> Some p then begin
        swap queue i p;
        up queue p
      end
    end

  let resize ({ capacity; elements; _ } as queue) =
    queue.capacity <- 2 * capacity;
    queue.elements <- Array.init (2 * capacity) (fun i ->
      if i < capacity then elements.(i) else None
    )

  let mem queue elt =
    EltHashtbl.mem queue.keys elt

  let get_weight queue elt =
    if not (mem queue elt) then begin
      Printf.printf "get_weight -> Not_found\n";
      raise Not_found
    end;
    EltHashtbl.find queue.weights elt

  let get_weight_opt queue elt =
    EltHashtbl.find_opt queue.weights elt

  let push queue elt wgt =
    if mem queue elt then raise Duplicate;

    if full queue then resize queue;

    let key = queue.size in

    queue.elements.(key) <- Some elt;

    EltHashtbl.add queue.weights elt wgt;
    EltHashtbl.add queue.keys elt key;

    queue.size <- queue.size + 1;

    up queue key

  let update queue elt wgt =
    match EltHashtbl.find_opt queue.weights elt with
    | None -> begin
        Printf.printf "update -> Not_found\n";
        raise Not_found
      end
    | Some old_wgt ->
        EltHashtbl.replace queue.weights elt wgt;
        let i = EltHashtbl.find queue.keys elt in
        if Wgt.compare old_wgt wgt <= 0 then
          down queue i
        else
          up queue i

  let pop_raw queue =
    let elt = Option.get queue.elements.(0) in
    let wgt = EltHashtbl.find queue.weights elt in

    swap queue 0 (queue.size - 1);

    queue.elements.(queue.size - 1) <- None;
    queue.size <- queue.size - 1;

    EltHashtbl.remove queue.weights elt;
    EltHashtbl.remove queue.keys elt;

    if 0 < queue.size then down queue 0;

    (elt, wgt)

  let pop queue =
    if queue.size = 0 then raise Empty;
    pop_raw queue

  let pop_opt queue =
    if queue.size = 0 then None else Some (pop_raw queue)
end


(* -------------------------------------------------------------------------- *)
(* Dijkstra. *)

module type GraphType = sig
  type t

  type vertice
  type distance = int

  include Hashtbl.HashedType with type t := vertice

  val vertices : t -> vertice list
  val neighbors : t -> vertice -> (vertice * int) list
end


module Dijkstra (G : GraphType) : sig

  val distances : G.t -> G.vertice -> (G.vertice * G.distance) Seq.t

  val distance_to_first : G.t -> G.vertice -> (G.vertice -> bool) -> int option
  val distance : G.t -> G.vertice -> G.vertice -> int option

end = struct

  module Vertice = struct
    type t = G.vertice
    let equal u v = G.equal u v
    let hash u = G.hash u
  end

  module VerticeHashtbl = Hashtbl.Make(Vertice)
  module MinQueue = PriorityQueue(Vertice)(ExtInt)

  let distances graph u =
    let vertices = G.vertices graph in
    let closest_vertices = MinQueue.create ~capacity:(List.length vertices) in

    List.iter (fun vertice ->
      MinQueue.push closest_vertices vertice ExtInt.PlusInf
    ) vertices;
    MinQueue.update closest_vertices u (ExtInt.Int 0);

    let distances = VerticeHashtbl.create (List.length vertices) in

    while not (MinQueue.empty closest_vertices) do
      let (w, dist) = MinQueue.pop closest_vertices in

      if dist <> PlusInf then begin
        let dist = ExtInt.get_int dist in

        VerticeHashtbl.add distances w dist;

        let neighbors = G.neighbors graph w in
        List.iter (fun (neighbor, edge_dist) ->
          if MinQueue.mem closest_vertices neighbor then begin
            match MinQueue.get_weight closest_vertices neighbor with
            | ExtInt.PlusInf ->
                  MinQueue.update closest_vertices neighbor (ExtInt.Int (edge_dist + dist))
            | ExtInt.Int neighbor_dist ->
                if edge_dist + dist < neighbor_dist then
                  MinQueue.update closest_vertices neighbor (ExtInt.Int (edge_dist + dist))
          end
        ) neighbors
      end

    done;

    VerticeHashtbl.to_seq distances


  let distance_to_first graph u p =
    let vertices = G.vertices graph in
    let distances = MinQueue.create ~capacity:(List.length vertices) in

    List.iter (fun vertice ->
      MinQueue.push distances vertice ExtInt.PlusInf
    ) vertices;

    MinQueue.update distances u (ExtInt.Int 0);

    let exception Found of int in
    try while not (MinQueue.empty distances) do
        let (w, dist) = MinQueue.pop distances in

        if dist <> PlusInf then begin
          let dist = ExtInt.get_int dist in
          if p w then raise (Found dist);

          let neighbors = G.neighbors graph w in
          List.iter (fun (neighbor, edge_dist) ->
            if MinQueue.mem distances neighbor then begin
              match MinQueue.get_weight distances neighbor with
              | ExtInt.PlusInf ->
                  MinQueue.update distances neighbor (ExtInt.Int (edge_dist + dist))
              | ExtInt.Int neighbor_dist ->
                  if edge_dist + dist < neighbor_dist then
                    MinQueue.update distances neighbor (ExtInt.Int (edge_dist + dist))
            end
          ) neighbors

        end
      done;
      None
    with Found dist ->
      Some dist

  let distance graph u v =
    distance_to_first graph u (G.equal v)

end
