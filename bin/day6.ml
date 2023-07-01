
open Core

let rec contains_duplicate str =
  if String.is_empty str then false else
  let (to_find, look_in) = String.get str 0, String.slice str 1 0
  in
  match String.find look_in ~f: (Char.equal to_find) with
  | Some _ -> true
  | None -> contains_duplicate look_in

let detect_packet_pos str window_size = 
  Sequence.range ~start:`inclusive ~stop:`exclusive window_size (String.length str)
  |> Sequence.find_exn ~f: ( fun stop ->
      let start = stop - window_size in
      let substr = String.slice str start stop in
      not (contains_duplicate substr)
  )

let () = 
  let input = In_channel.read_all "input/day6.txt" in
  print_endline (Int.to_string (detect_packet_pos input 4));
  print_endline (Int.to_string (detect_packet_pos input 14));
