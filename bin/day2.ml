open Core

type rps_move = Rock | Paper | Scissors
type outcome = Win | Loss | Draw

let score_round (a, b) =
  let match_result =
    match a with
    | Rock -> ( match b with Rock -> 3 | Paper -> 6 | Scissors -> 0)
    | Paper -> ( match b with Rock -> 0 | Paper -> 3 | Scissors -> 6)
    | Scissors -> ( match b with Rock -> 6 | Paper -> 0 | Scissors -> 3)
  in
  (match b with Rock -> 1 | Paper -> 2 | Scissors -> 3) + match_result

let move_of_str_a move_str =
  let c = String.get move_str 0 in
  match c with
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors 
  | _ -> raise (Stdlib.Invalid_argument ("Unknown string" ^ String.of_char c))

let move_of_str_b move_str =
  let c = String.get move_str 0 in
  match c with
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissors 
  | _ -> raise (Stdlib.Invalid_argument ("Unknown string" ^ String.of_char c))

let outcome_of_str outcome_str =
  let c = String.get outcome_str 0 in
  match c with
  | 'X' -> Loss
  | 'Y' -> Draw 
  | 'Z' -> Win 
  | _ -> raise (Stdlib.Invalid_argument ("Unknown string" ^ String.of_char c))


let parse_line line =
  let left, right = String.lsplit2_exn line ~on:' ' in
  let left_move = move_of_str_a left in
  let right_move = move_of_str_b right in
  (left_move, right_move)

let parse_outcome_line line =
  let left, right = String.lsplit2_exn line ~on:' ' in
  let left_move = move_of_str_a left in
  let outcome = outcome_of_str right in
  (left_move, outcome)

let moves_from_outcome (move, outcome) =
  let my_move = 
    match outcome with
    | Draw -> (match move with Rock -> Rock | Paper -> Paper | Scissors -> Scissors)
    | Win -> (match move with Rock -> Paper | Paper -> Scissors | Scissors -> Rock)
    | Loss -> (match move with Rock -> Scissors | Paper -> Rock | Scissors -> Paper)
  in
    (move, my_move)

let () =
  let initial_score =
    In_channel.read_lines "input/day2.txt"
    |> List.map ~f:parse_line |> List.map ~f:score_round
    |> List.reduce_exn ~f:( + )
  in
  print_endline (string_of_int initial_score);

  let final_score =
    In_channel.read_lines "input/day2.txt"
    |> List.map ~f:parse_outcome_line
    |> List.map ~f:moves_from_outcome
    |>  List.map ~f:score_round
    |> List.reduce_exn ~f:( + )
  in
  print_endline (string_of_int final_score);
