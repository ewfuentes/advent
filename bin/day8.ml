open Core

type grid = int array array
type directional_grid = { top : grid; left : grid; bottom : grid; right : grid }

let parse_lines lines =
  let num_rows = List.length lines in
  let num_cols = String.length (List.hd_exn lines) in
  let grid = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0 in
  List.iteri lines ~f:(fun row_idx line ->
      String.iteri line ~f:(fun col_idx elem ->
          grid.(row_idx).(col_idx) <- Char.get_digit_exn elem));
  grid

let compute_visibility grid =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let out =
    {
      top = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0;
      left = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0;
      bottom = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0;
      right = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0;
    }
  in
  Array.iteri grid ~f:(fun row_idx row ->
      Array.iteri row ~f:(fun col_idx _ ->
          if row_idx = 0 then (
            out.top.(row_idx).(col_idx) <- -1;
            out.bottom.(num_rows - 1).(col_idx) <- -1)
          else
            (* From top *)
            let tree_height_above = grid.(row_idx - 1).(col_idx) in
            let prev_max_above = out.top.(row_idx - 1).(col_idx) in
            out.top.(row_idx).(col_idx) <-
              Int.max tree_height_above prev_max_above;

            (* From bottom *)
            let tree_height_below = grid.(num_rows - row_idx).(col_idx) in
            let prev_max_below = out.bottom.(num_rows - row_idx).(col_idx) in
            out.bottom.(num_rows - row_idx - 1).(col_idx) <-
              Int.max tree_height_below prev_max_below;

            if col_idx = 0 then (
              out.left.(row_idx).(col_idx) <- -1;
              out.right.(row_idx).(num_cols - 1) <- -1)
            else
              (* From left *)
              let tree_height_left = grid.(row_idx).(col_idx - 1) in
              let prev_max_left = out.left.(row_idx).(col_idx - 1) in
              out.left.(row_idx).(col_idx) <-
                Int.max tree_height_left prev_max_left;

              (* From right *)
              let tree_height_right = grid.(row_idx).(num_cols - col_idx) in
              let prev_max_right = out.right.(row_idx).(num_cols - col_idx) in
              out.right.(row_idx).(num_cols - col_idx - 1) <-
                (if col_idx = 0 then -1
                 else Int.max tree_height_right prev_max_right)));
  out

let compute_scenic_score_for_cell grid row_idx col_idx =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  (* Compute top *)
  let my_cell_height = grid.(row_idx).(col_idx) in
  let top_score =
    let seq =
      Sequence.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive row_idx 0
    in
    Sequence.fold_until seq ~init:0
      ~f:(fun _ check_row ->
        let check_cell_height = grid.(check_row).(col_idx) in
        if my_cell_height > check_cell_height then Continue (row_idx - check_row)
        else Stop (row_idx - check_row))
      ~finish:(fun _ -> row_idx)
  in
  (* Compute left *)
  let left_score =
    let seq =
      Sequence.range ~stride:(-1) ~start:`exclusive ~stop:`inclusive col_idx 0
    in
    Sequence.fold_until seq ~init:0
      ~f:(fun _ check_col ->
        let check_cell_height = grid.(row_idx).(check_col) in
        if my_cell_height > check_cell_height then Continue (col_idx - check_col)
        else Stop (col_idx - check_col))
      ~finish:(fun _ -> col_idx)
  in
  (* Compute right *)
  let right_score =
    let seq =
      Sequence.range ~start:`exclusive ~stop:`exclusive col_idx num_cols
    in
    Sequence.fold_until seq ~init:0
      ~f:(fun _ check_col ->
        let check_cell_height = grid.(row_idx).(check_col) in
        (*print_endline (
            "check_col: " ^ (Int.to_string check_col) ^
            " height: " ^ (Int.to_string check_cell_height) ^
            " score: " ^ (Int.to_string (check_col - col_idx))
          );*)
        if my_cell_height > check_cell_height then Continue (check_col - col_idx)
        else Stop (check_col - col_idx))
      ~finish:(fun _ -> num_cols - col_idx - 1)
  in
  (* Compute bottom *)
  let bottom_score =
    let seq =
      Sequence.range ~start:`exclusive ~stop:`exclusive row_idx num_rows
    in
    Sequence.fold_until seq ~init:0
      ~f:(fun _ check_row ->
        let check_cell_height = grid.(check_row).(col_idx) in
        if my_cell_height > check_cell_height then Continue (check_row - row_idx)
        else Stop (check_row - row_idx))
      ~finish:(fun _ -> num_rows - row_idx - 1)
  in
  top_score * left_score * right_score * bottom_score

let compute_scenic_scores grid =
  let num_rows = Array.length grid in
  let num_cols = Array.length grid.(0) in
  let out = Array.make_matrix ~dimx:num_rows ~dimy:num_cols 0 in
  Array.iteri out ~f:(fun row_idx row ->
      Array.iteri row ~f:(fun col_idx _ ->
          out.(row_idx).(col_idx) <-
            compute_scenic_score_for_cell grid row_idx col_idx));
  out

let compute_num_visible grid vis =
  Array.foldi grid ~init:0 ~f:(fun row_idx accum row ->
      let row_sum =
        Array.foldi row ~init:0 ~f:(fun col_idx accum elem ->
            let is_hidden =
              elem <= vis.top.(row_idx).(col_idx)
              && elem <= vis.bottom.(row_idx).(col_idx)
              && elem <= vis.left.(row_idx).(col_idx)
              && elem <= vis.right.(row_idx).(col_idx)
            in
            (if is_hidden then 0 else 1) + accum)
      in
      row_sum + accum)

let compute_max_score scores =
  Array.fold scores ~init:0 ~f:(fun accum row ->
      Array.fold row ~init:accum ~f:(fun accum score -> Int.max accum score))

let () =
  let lines = In_channel.read_lines "input/day8.txt" in
  let grid = parse_lines lines in
  let visibility = compute_visibility grid in
  let num_visible = compute_num_visible grid visibility in
  print_endline (Int.to_string num_visible);
  let scores = compute_scenic_scores grid in
  let max_score = compute_max_score scores in
  print_endline (Int.to_string max_score)
