open Core

type packet = Value of int | Packet of packet list

let rec sexp_to_packet = function
  | Sexp.Atom s -> Value (Int.of_string s)
  | Sexp.List l -> Packet (List.map l ~f:sexp_to_packet)

let rec packet_to_sexp = function
  | Value i -> Sexp.Atom (Int.to_string i)
  | Packet l -> Sexp.List (List.map l ~f:packet_to_sexp)

let parse_packet line =
  if String.is_empty line then None
  else
    let packet =
      String.substr_replace_all ~pattern:"[" ~with_:"(" line
      |> String.substr_replace_all ~pattern:"]" ~with_:")"
      |> String.substr_replace_all ~pattern:"," ~with_:" "
      |> Sexp.of_string |> sexp_to_packet
    in
    Some packet

let rec compare (a : packet) (b : packet) =
  match (a, b) with
  | Value _, _ -> failwith "Left had only Value"
  | _, Value _ -> failwith "Right had only Value"
  | Packet [], Packet [] -> 0
  | Packet [], Packet (_ :: _) -> -1 (* left ran out first *)
  | Packet (_ :: _), Packet [] -> 1 (* right ran out first *)
  | Packet (Value l :: lrest), Packet (Value r :: rrest) ->
      if l < r then -1
      else if r < l then 1
      else compare (Packet lrest) (Packet rrest)
  | Packet (Value l :: lrest), Packet (Packet r :: rrest) ->
      let result = compare (Packet [ Value l ]) (Packet r) in
      if result <> 0 then result else compare (Packet lrest) (Packet rrest)
  | Packet (Packet l :: lrest), Packet (Value r :: rrest) ->
      let result = compare (Packet l) (Packet [ Value r ]) in
      if result <> 0 then result else compare (Packet lrest) (Packet rrest)
  | Packet (Packet l :: lrest), Packet (Packet r :: rrest) ->
      let result = compare (Packet l) (Packet r) in
      if result <> 0 then result else compare (Packet lrest) (Packet rrest)

let () =
  In_channel.read_lines "input/day13.txt"
  |> List.filter_map ~f:(fun line -> parse_packet line)
  |> List.chunks_of ~length:2
  |> List.map ~f:(fun a -> (List.hd_exn a, List.last_exn a))
  |> List.mapi ~f:(fun idx (a, b) ->
         let result = compare a b in
         if result < 0 then idx + 1 else 0)
  |> List.reduce_exn ~f:( + ) |> Int.to_string |> print_endline;

  let divider_packet_1 = sexp_to_packet (Sexp.of_string "((2))") in
  let divider_packet_2 = sexp_to_packet (Sexp.of_string "((6))") in
  let div_packets = [ divider_packet_1; divider_packet_2 ] in
  In_channel.read_lines "input/day13.txt"
  |> List.filter_map ~f:(fun line -> parse_packet line)
  |> List.append div_packets |> List.sort ~compare
  |> List.filter_mapi ~f:(fun idx p ->
         if
           Sexp.equal (packet_to_sexp p) (packet_to_sexp divider_packet_1)
           || Sexp.equal (packet_to_sexp p) (packet_to_sexp divider_packet_2)
         then Some (idx + 1)
         else None)
  |> List.reduce_exn ~f:( * ) |> Int.to_string |> print_endline
