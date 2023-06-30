open Core

type rps_move = Rock | Paper | Scissors
type outcome = Win | Loss | Draw

let score_round (a, b) =
  let match_result =
    match (a, b) with
    | Paper, Scissors | Scissors, Rock | Rock, Paper -> 6
    | Rock, Rock | Paper, Paper | Scissors, Scissors -> 3
    | Scissors, Paper | Paper, Rock | Rock, Scissors -> 0
  in
  (match b with Rock -> 1 | Paper -> 2 | Scissors -> 3) + match_result

let move_of_str_a move_str =
  let c = String.get move_str 0 in
  match c with
  | 'A' -> Rock
  | 'B' -> Paper
  | 'C' -> Scissors
  | _ -> failwith ("Unknown string" ^ Char.to_string c)

let move_of_str_b move_str =
  let c = String.get move_str 0 in
  match c with
  | 'X' -> Rock
  | 'Y' -> Paper
  | 'Z' -> Scissors
  | _ -> failwith ("Unknown string" ^ Char.to_string c)

let outcome_of_str outcome_str =
  let c = String.get outcome_str 0 in
  match c with
  | 'X' -> Loss
  | 'Y' -> Draw
  | 'Z' -> Win
  | _ -> failwith ("Unknown string" ^ Char.to_string c)

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
    match (move, outcome) with
    | Rock, Draw | Paper, Loss | Scissors, Win -> Rock
    | Rock, Win | Paper, Draw | Scissors, Loss -> Paper
    | Rock, Loss | Paper, Win | Scissors, Draw -> Scissors
  in
  (move, my_move)

let () =
  let lines = In_channel.read_lines "input/day2.txt" in
  let initial_score =
    lines |> List.map ~f:parse_line |> List.map ~f:score_round
    |> List.reduce_exn ~f:( + )
  in
  print_endline (string_of_int initial_score);
  let final_score =
    lines
    |> List.map ~f:parse_outcome_line
    |> List.map ~f:moves_from_outcome
    |> List.map ~f:score_round |> List.reduce_exn ~f:( + )
  in
  print_endline (string_of_int final_score)
