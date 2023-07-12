open Core

module Loc = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, of_sexp]
  end

  include T
  include Comparable.Make (T)

  let add (x_a, y_a) (x_b, y_b) = (x_a + x_b, y_a + y_b)
  let ( + ) = add
  let mult a (x, y) = (a * x, a * y)
  let ( * ) = mult
  let sub a b = a + (-1 * b)
  let ( - ) = sub
  let equal (a : t) (b : t) = sub a b = (0, 0)
  let ( = ) = equal
  let to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"

  let clamp (x, y) = let c = Int.clamp_exn ~min:(-1) ~max:(1) in (c x), (c y)
end

type segment = Loc.t * Loc.t

let parse_line line : (Loc.t * Loc.t) list =
  let re = Re2.create_exn {|(?P<first>\d+),(?P<last>\d+)|} in
  let endpoints =
    Re2.get_matches_exn re line
    |> List.map ~f:(fun m ->
           let first = Re2.Match.get_exn ~sub:(`Name "first") m in
           let first = Int.of_string first in
           let last = Re2.Match.get_exn ~sub:(`Name "last") m in
           let last = Int.of_string last in
           (first, last))
  in
  List.map2_exn (List.slice endpoints 0 (-1)) (List.slice endpoints 1 0)
    ~f:(fun a b -> (a, b))

let create_world (segments : segment list) =
  List.fold segments
    ~init:(Set.empty (module Loc))
    ~f:(fun world (start, stop) ->
      let delta = Loc.clamp (Loc.(stop - start)) in
      let rec helper world loc =
        let new_world = Set.add world loc in
        if Loc.(loc = stop) then new_world
        else helper new_world Loc.(loc + delta)
      in
      helper world start)

type extrema = { left : int; right : int; top : int; bottom : int }

let draw_world world =
  let extrema =
    Set.fold world
      ~init:{ left = Int.max_value; right = 0; top = Int.max_value; bottom = 0 }
      ~f:(fun extrema (col, row) -> 
        {
          left = Int.min extrema.left col;
          right= Int.max extrema.right col;
          top = Int.min extrema.top row;
          bottom = Int.max extrema.bottom row;
        })
  in

  let rows =
    Sequence.range ~start:`inclusive ~stop:`inclusive extrema.top extrema.bottom
  in
  let cols =
    Sequence.range ~start:`inclusive ~stop:`inclusive extrema.left extrema.right
  in
  Sequence.iter rows ~f:(fun row ->
      Sequence.iter cols ~f:(fun col ->
          let char = if Set.mem world (col, row) then '#' else '.' in
          Out_channel.output_char stdout char);
      Out_channel.newline stdout);
  world

let parse_lines lines =
  List.map lines ~f:(fun line -> parse_line line) |> List.join |> create_world

let sim_step ~(floor: int option) world (x, y) =
  let floor_value = Option.value_or_thunk ~default: (fun () -> Int.max_value) floor in
  let is_invalid (x, y) = 
    (Set.mem world (x, y)) || (floor_value = y)
  in
  if not (is_invalid (x, y + 1)) then (true, (x, y + 1))
  else if not (is_invalid (x - 1, y + 1)) then (true, (x - 1, y + 1))
  else if not (is_invalid (x + 1, y + 1)) then (true, (x + 1, y + 1))
  else (false, (x, y))

type stepper_result = Settle of Loc.t | Exile | SourceBlocked

let compute_capacity ?(spawn = (500, 0)) ?(has_floor = false) world =
  let max_height =
    Set.fold ~init:0
      ~f:(fun max_height (_, height) -> Int.max max_height height)
      world
  in
  let floor_height = if has_floor then Some (max_height + 2) else None in
  let rec particle_counter ?(count = 0) world =
    let rec stepper world particle =
      if Set.mem world particle then SourceBlocked
      else
      let did_update, (x_new, y_new) = sim_step world particle ~floor:floor_height in
      if not did_update then Settle (x_new, y_new)
      else if (not has_floor) && (y_new > max_height) then Exile
      else stepper world (x_new, y_new)
    in
    match stepper world spawn with
    | Settle loc ->
        let new_world = Set.add world loc in
        particle_counter ~count:(count + 1) new_world
    | Exile -> count
    | SourceBlocked -> count
  in
  particle_counter world

let () =
  let world = In_channel.read_lines "input/day14.txt"
  |> parse_lines in
  world |> compute_capacity |> Int.to_string |> print_endline;
  world |> compute_capacity ~has_floor:true |> Int.to_string |> print_endline;
