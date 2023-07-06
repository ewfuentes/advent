open Core

type monkey = {
  id : int;
  mutable items : int list;
  op : int -> int;
  test : int -> bool;
  divisor : int;
  true_target : int;
  false_target : int;
  mutable inspection_count : int;
}

let parse_id lines : int =
  List.hd_exn lines |> String.split ~on:' ' |> List.last_exn
  |> String.chop_suffix_exn ~suffix:":"
  |> Int.of_string

let parse_items lines =
  let re = Re2.create_exn {|(\d+),?|} in
  List.nth_exn lines 1 |> Re2.get_matches_exn re
  |> List.map ~f:(fun m ->
         let match_str = Re2.Match.get_exn ~sub:(`Index 1) m in
         Int.of_string match_str)

let parse_op lines =
  let re = Re2.create_exn {|new = old (\*|\+) (.*)$|} in
  let m = List.nth_exn lines 2 |> Re2.first_match_exn re in
  let op =
    match Re2.Match.get_exn ~sub:(`Index 1) m with
    | "+" -> ( + )
    | "*" -> ( * )
    | x -> failwith ("Unknown op: " ^ x)
  in
  let value_str = Re2.Match.get_exn ~sub:(`Index 2) m in
  fun x ->
    let value =
      match value_str with "old" -> x | int_str -> Int.of_string int_str
    in
    op x value

let parse_test lines =
  let re = Re2.create_exn {|Test: divisible by (\d+)|} in
  let line = List.nth_exn lines 3 in
  let m = Re2.first_match_exn re line in
  let divisor_str = Re2.Match.get_exn ~sub:(`Index 1) m in
  let divisor = Int.of_string divisor_str in
  ((fun x -> x mod divisor = 0), divisor)

let parse_true_target lines =
  let re = Re2.create_exn {|If true: throw to monkey (\d+)|} in
  let line = List.nth_exn lines 4 in
  let m = Re2.first_match_exn re line in
  let monkey_id = Re2.Match.get_exn ~sub:(`Index 1) m in
  Int.of_string monkey_id

let parse_false_target lines =
  let re = Re2.create_exn {|If false: throw to monkey (\d+)|} in
  let line = List.nth_exn lines 5 in
  let m = Re2.first_match_exn re line in
  let monkey_id = Re2.Match.get_exn ~sub:(`Index 1) m in
  Int.of_string monkey_id

let parse_monkey lines =
  let id = parse_id lines in
  let test, divisor = parse_test lines in
  {
    id;
    items = parse_items lines;
    op = parse_op lines;
    test;
    divisor;
    true_target = parse_true_target lines;
    false_target = parse_false_target lines;
    inspection_count = 0;
  }

let parse_lines lines =
  lines |> List.chunks_of ~length:7 |> List.map ~f:parse_monkey

let advance ~worry_divisor monkeys monkey_id : unit =
  let mut = List.nth_exn monkeys monkey_id in
  let true_monkey = List.nth_exn monkeys mut.true_target in
  let false_monkey = List.nth_exn monkeys mut.false_target in

  let mod_value =
    List.fold monkeys ~init:1 ~f:(fun accum monkey -> accum * monkey.divisor)
  in

  mut.inspection_count <- mut.inspection_count + List.length mut.items;

  List.iter mut.items ~f:(fun worry ->
      let new_worry = mut.op worry / worry_divisor mod mod_value in
      if mut.test new_worry then
        true_monkey.items <- List.append true_monkey.items [ new_worry ]
      else false_monkey.items <- List.append false_monkey.items [ new_worry ]);

  mut.items <- []

let advance_all ?(num_rounds = 1) ?(worry_divisor = 3) monkeys : unit =
  let num_monkeys = List.length monkeys in
  Sequence.range ~stop:`inclusive 1 num_rounds
  |> Sequence.iter ~f:(fun _ ->
         Sequence.range 0 num_monkeys
         |> Sequence.iter ~f:(fun id ->
                advance ~worry_divisor monkeys id;
                ()))

let () =
  let lines = In_channel.read_lines "input/day11.txt" in
  let monkeys = parse_lines lines in
  advance_all ~num_rounds:20 monkeys;
  let sorted_monkeys =
    List.sort monkeys ~compare:(fun a b ->
        b.inspection_count - a.inspection_count)
  in
  let a, b = (List.hd_exn sorted_monkeys, List.nth_exn sorted_monkeys 1) in
  print_endline (Int.to_string (a.inspection_count * b.inspection_count));

  let monkeys = parse_lines lines in
  advance_all ~worry_divisor:1 ~num_rounds:10000 monkeys;
  let sorted_monkeys =
    List.sort monkeys ~compare:(fun a b ->
        b.inspection_count - a.inspection_count)
  in
  let a, b = (List.hd_exn sorted_monkeys, List.nth_exn sorted_monkeys 1) in
  print_endline (Int.to_string (a.inspection_count * b.inspection_count))
