open Core

type inst = Noop | Addx of int
type cpu_state = int

let parse_lines lines =
  List.map lines ~f:(fun line ->
      match String.split ~on:' ' line with
      | [ "noop" ] -> Noop
      | [ "addx"; x ] -> Addx (Int.of_string x)
      | _ -> failwith ("Unknown instruction " ^ line))

let advance state inst =
  match inst with Noop -> (1, state) | Addx x -> (2, state + x)

let value_at_cycle trace cycle =
  let _, value =
    List.find_exn trace ~f:(fun (cycle_count, _) -> cycle_count <= cycle)
  in
  value

let compute_signal_strength (trace : (int * cpu_state) list) =
  print_endline (Int.to_string (fst (List.hd_exn trace)));
  Sequence.range ~start:`inclusive ~stop:`inclusive ~stride:40 20
    (fst (List.hd_exn trace))
  |> Sequence.fold ~init:0 ~f:(fun accum query_cycle ->
         let state = value_at_cycle trace query_cycle in
         print_endline
           ("query cycle: " ^ Int.to_string query_cycle ^ " "
          ^ Int.to_string state);
         accum + (query_cycle * state))

let print_crt trace =
  Sequence.range ~start:`inclusive ~stop:`inclusive ~stride:40 0 200
  |> Sequence.iter ~f:(fun start_idx ->
         Sequence.range ~start:`inclusive ~stop:`exclusive 0 40
         |> Sequence.iter ~f:(fun pixel ->
                let value = value_at_cycle trace (pixel + start_idx + 1) in
                if Int.abs (pixel - value) <= 1 then
                  Out_channel.output_char stdout '#'
                else Out_channel.output_char stdout '.');
         Out_channel.newline stdout)

let () =
  let trace =
    In_channel.read_lines "input/day10.txt"
    |> parse_lines
    |> List.fold
         ~init:[ (1, 1) ]
         ~f:(fun accum inst ->
           let cycle, state = List.hd_exn accum in
           let cycles_ticked, new_state = advance state inst in
           (cycles_ticked + cycle, new_state) :: accum)
  in
  compute_signal_strength trace |> Int.to_string |> print_endline;

  print_crt trace
