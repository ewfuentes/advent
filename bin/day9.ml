open Core

module Dir = struct
  type t = Up | Down | Left | Right

  let to_string = function
    | Up -> "Up"
    | Down -> "Down"
    | Left -> "Left"
    | Right -> "Right"
end

module Loc = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, of_sexp]
  end

  include T
  include Comparable.Make (T)

  let add (x_a, y_a) (x_b, y_b) = (x_a + x_b, y_a + y_b)

  let dist (x_a, y_a) (x_b, y_b) =
    Int.max (Int.abs (x_a - x_b)) (Int.abs (y_a - y_b))

  let to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"
end

module Rope = struct
  type t = Loc.t list

  let make n =
    let rec helper r remaining =
      if remaining = 0 then r else helper ((0, 0) :: r) (remaining - 1)
    in
    helper [] n

  let to_string r =
    let rec helper r out =
      match r with
      | [] -> out
      | hd :: rest -> helper rest (out ^ Loc.to_string hd)
    in
    helper r ""
end

let parse_lines lines =
  List.map lines ~f:(fun line ->
      match String.split line ~on:' ' with
      | [ "U"; n ] -> (Dir.Up, Int.of_string n)
      | [ "D"; n ] -> (Dir.Down, Int.of_string n)
      | [ "L"; n ] -> (Dir.Left, Int.of_string n)
      | [ "R"; n ] -> (Dir.Right, Int.of_string n)
      | _ -> failwith ("Unknown move " ^ line))

let rec advance state (dir, count) tail_set =
  if count = 0 then (state, tail_set)
  else
    let dloc =
      match dir with
      | Dir.Up -> (0, 1)
      | Dir.Down -> (0, -1)
      | Dir.Left -> (-1, 0)
      | Dir.Right -> (1, 0)
    in
    let rec advance_body to_update new_body =
      match (to_update, new_body) with
      | [], rev_body -> List.rev rev_body
      | _, [] -> failwith "empty new body"
      | old_next :: rest, new_prev :: new_rest ->
          let new_next =
            if Loc.dist new_prev old_next < 2 then old_next
            else
              let (x_p, y_p), (x_n, y_n) = (new_prev, old_next) in
              let clamp = Int.clamp_exn ~min:(-1) ~max:1 in
              let dloc = (clamp (x_p - x_n), clamp (y_p - y_n)) in
              Loc.add old_next dloc
          in
          advance_body rest (new_next :: new_prev :: new_rest)
    in
    let new_head = Loc.add (List.hd_exn state) dloc in
    let new_state = advance_body (List.tl_exn state) [ new_head ] in
    let new_tail_set = Set.add tail_set (List.last_exn new_state) in
    advance new_state (dir, count - 1) new_tail_set

let () =
  let lines = In_channel.read_lines "input/day9.txt" in
  let moves = parse_lines lines in
  let _, tail_set =
    List.fold moves
      ~init:(Rope.make 2, Set.empty (module Loc))
      ~f:(fun (state, tail_set) move ->
        let new_state, new_tail_set = advance state move tail_set in
        (new_state, new_tail_set))
  in
  print_endline (Int.to_string (Set.length tail_set));
  let _, tail_set =
    List.fold moves
      ~init:(Rope.make 10, Set.empty (module Loc))
      ~f:(fun (state, tail_set) move ->
        let new_state, new_tail_set = advance state move tail_set in
        (new_state, new_tail_set))
  in
  print_endline (Int.to_string (Set.length tail_set))
