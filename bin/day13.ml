
open Core

type packet = Value of int | Packet of packet list

let rec sexp_to_packet = function
  | Sexp.Atom s -> Value (Int.of_string s)
  | Sexp.List l -> Packet (List.map l ~f:sexp_to_packet)


let parse_packet line =
  if String.is_empty line then
    None
  else
    let packet_str = String.substr_replace_all ~pattern:"[" ~with_:"(" line in
    let packet_str = String.substr_replace_all ~pattern:"]" ~with_:")" packet_str in
    let packet_str = String.substr_replace_all ~pattern:"," ~with_:" " packet_str in
    let sexp = Sexp.of_string packet_str in
    let packet = sexp_to_packet sexp in
    Some packet


let parse_lines lines: packet list list =
  let packets = List.filter_map lines ~f:(
    fun line -> parse_packet line
  ) in
  List.chunks_of ~length: 2 packets
  


let () =
  let lines = In_channel.read_lines "input/day13_test.txt" in
  let packet_pairs = parse_lines lines in
  print_endline "Hello World!"
