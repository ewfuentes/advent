open Core

type travel_dir = Uphill | Downhill

module Loc = struct
  module T = struct
    type t = int * int [@@deriving compare, sexp_of, of_sexp]
  end

  include T
  include Comparable.Make (T)

  let to_string (x, y) = "(" ^ Int.to_string x ^ ", " ^ Int.to_string y ^ ")"
  let add (x_a, y_a) (x_b, y_b) = (x_a + x_b, y_a + y_b)

  let dist (x_a, y_a) (x_b, y_b) =
    Int.max (Int.abs (x_a - x_b)) (Int.abs (y_a - y_b))
end

let parse_lines lines =
  let num_rows = List.length lines in
  let num_cols = String.length (List.hd_exn lines) in
  let out = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0 in
  let start = ref (0, 0) in
  let stop = ref (0, 0) in
  List.iteri lines ~f:(fun row_idx row ->
      String.iteri row ~f:(fun col_idx char ->
          let height_char =
            if Char.equal char 'S' then (
              start := (row_idx, col_idx);
              'a')
            else if Char.equal char 'E' then (
              stop := (row_idx, col_idx);
              'z')
            else char
          in
          out.(row_idx).(col_idx) <- Char.to_int height_char - Char.to_int 'a'));
  (out, !start, !stop)

let print_array cost_map =
  Array.iter cost_map ~f:(fun row ->
      Array.iter row ~f:(fun item -> Out_channel.printf " %04d" item);
      Out_channel.newline stdout)

let djikstra ?(step_cost = 1) ?(travel_dir = Uphill) grid (x_a, y_a) =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let out = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 1000 in
  out.(x_a).(y_a) <- 0;
  let rec update_helper cost_map : int array array =
    let out = Array.copy cost_map in
    let did_update = ref false in
    Array.iteri grid ~f:(fun row_idx row ->
        Array.iteri row ~f:(fun col_idx item ->
            let min_height_delta, max_height_delta =
              match travel_dir with
              | Uphill -> (Int.min_value, 1)
              | Downhill -> (-1, Int.max_value)
            in
            (* check neighbors *)
            (* Check above *)
            (if row_idx > 0 then
               let height_delta = item - grid.(row_idx - 1).(col_idx) in
               let cost_delta =
                 out.(row_idx).(col_idx) - out.(row_idx - 1).(col_idx)
               in
               if
                 Int.between ~low:min_height_delta ~high:max_height_delta
                   height_delta
                 && not (cost_delta <= step_cost)
               then (
                 did_update := true;
                 out.(row_idx).(col_idx) <-
                   out.(row_idx - 1).(col_idx) + step_cost));

            (* Check below *)
            (if row_idx < num_rows - 1 then
               let height_delta = item - grid.(row_idx + 1).(col_idx) in
               let cost_delta =
                 out.(row_idx).(col_idx) - out.(row_idx + 1).(col_idx)
               in
               if
                 Int.between ~low:min_height_delta ~high:max_height_delta
                   height_delta
                 && not (cost_delta <= step_cost)
               then (
                 did_update := true;
                 out.(row_idx).(col_idx) <-
                   out.(row_idx + 1).(col_idx) + step_cost));
            (* Check right *)
            (if col_idx < num_cols - 1 then
               let height_delta = item - grid.(row_idx).(col_idx + 1) in
               let cost_delta =
                 out.(row_idx).(col_idx) - out.(row_idx).(col_idx + 1)
               in
               if
                 Int.between ~low:min_height_delta ~high:max_height_delta
                   height_delta
                 && not (cost_delta <= step_cost)
               then (
                 did_update := true;
                 out.(row_idx).(col_idx) <-
                   out.(row_idx).(col_idx + 1) + step_cost));
            (* Check left *)
            if col_idx > 0 then
              let height_delta = item - grid.(row_idx).(col_idx - 1) in
              let cost_delta =
                out.(row_idx).(col_idx) - out.(row_idx).(col_idx - 1)
              in
              if
                Int.between ~low:min_height_delta ~high:max_height_delta
                  height_delta
                && not (cost_delta <= step_cost)
              then (
                did_update := true;
                out.(row_idx).(col_idx) <-
                  out.(row_idx).(col_idx - 1) + step_cost)));
    if not !did_update then out else update_helper out
  in
  update_helper out

let find_nearest_low_point grid value_func =
  let min_loc = ref (0, 0) in
  let min_val = ref 1000 in
  Array.iteri grid ~f:(fun row_idx row ->
      Array.iteri row ~f:(fun col_idx item ->
          let value = value_func.(row_idx).(col_idx) in
          if item = 0 && value < !min_val then (
            min_loc := (row_idx, col_idx);
            min_val := value)));
  (!min_loc, !min_val)

let () =
  let lines = In_channel.read_lines "input/day12.txt" in
  let grid, start_loc, stop_loc = parse_lines lines in
  let value_func = djikstra grid start_loc in
  let stop_x, stop_y = stop_loc in
  print_endline (Int.to_string value_func.(stop_x).(stop_y));
  let value_func = djikstra ~travel_dir:Downhill grid stop_loc in
  let _, lowest_value = find_nearest_low_point grid value_func in
  print_endline (Int.to_string lowest_value)
