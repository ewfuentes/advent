open Core

type range = { intial : int; final : int }

let range_length r = r.final - r.intial + 1

let parse_range str =
  let min, max = String.lsplit2_exn str ~on:'-' in
  { intial = Int.of_string min; final = Int.of_string max }

let parse_ranges line =
  let first, second = String.lsplit2_exn line ~on:',' in
  (parse_range first, parse_range second)

let is_included_interval (interval_1, interval_2) =
  let a, b =
    if range_length interval_1 <= range_length interval_2 then
      (interval_1, interval_2)
    else (interval_2, interval_1)
  in
  b.intial <= a.intial && b.final >= a.final

let is_overlapping_intervals (interval_1, interval_2) =
  let a, b =
    if interval_1.intial <= interval_2.intial then (interval_1, interval_2)
    else (interval_2, interval_1)
  in
  a.final >= b.intial

let () =
  let lines = In_channel.read_lines "input/day4.txt" in
  let intervals = List.map lines ~f:parse_ranges in
  intervals
  |> List.filter ~f:is_included_interval
  |> List.length |> Int.to_string |> print_endline;

  intervals
  |> List.filter ~f:is_overlapping_intervals
  |> List.length |> Int.to_string |> print_endline
