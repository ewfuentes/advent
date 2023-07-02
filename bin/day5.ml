open Core

type move = { src : int; dst : int; count : int }

(*
let move_to_string move =
  "<Move src: " ^ Int.to_string move.src ^ " dst: " ^ Int.to_string move.dst
  ^ " count: " ^ Int.to_string move.count ^ ">"

let stack_to_string (stack : char Stack.t) =
  Stack.fold stack ~init:"" ~f:(fun acc item -> Char.to_string item ^ acc)

let stacks_to_string stacks =
  List.mapi stacks ~f:(fun idx stack ->
      Int.to_string (idx + 1) ^ ": " ^ stack_to_string stack)
  |> String.concat ~sep:"\n"

*)

let extract_stack (boxes : string list) (idx : int) =
  List.fold boxes ~init:(Stack.create ()) ~f:(fun stack line ->
      let c = String.get line idx in
      if Char.is_alpha c then Stack.push stack c;
      stack)

let parse_state lines =
  let rev_lines = List.rev lines in
  let labels, boxes = (List.hd_exn rev_lines, List.tl_exn rev_lines) in
  let rev_idxs =
    String.foldi labels ~init:[] ~f:(fun idx accum char ->
        if Char.is_digit char then idx :: accum else accum)
  in
  let idxs = List.rev rev_idxs in
  List.map idxs ~f:(extract_stack boxes)

let parse_move line =
  let re =
    Re2.create_exn {|move (?P<count>\d+) from (?P<src>\d+) to (?P<dst>\d+)|}
  in
  let m = Re2.first_match_exn re line in
  {
    src = Int.of_string (Re2.Match.get_exn ~sub:(`Name "src") m);
    dst = Int.of_string (Re2.Match.get_exn ~sub:(`Name "dst") m);
    count = Int.of_string (Re2.Match.get_exn ~sub:(`Name "count") m);
  }

let parse_moves lines = List.map lines ~f:parse_move

let parse_puzzle lines =
  let split_idx, _ =
    List.findi_exn lines ~f:(fun _ elem -> String.equal elem "")
  in
  let state_strs, move_strs =
    (List.slice lines 0 split_idx, List.slice lines (split_idx + 1) 0)
  in
  (parse_state state_strs, parse_moves move_strs)

let rec apply_move stacks move =
  if Int.equal move.count 0 then stacks
  else
    let src = List.nth_exn stacks (move.src - 1) in
    let dst = List.nth_exn stacks (move.dst - 1) in
    Stack.pop_exn src |> Stack.push dst;
    let new_stacks =
      List.mapi stacks ~f:(fun idx existing ->
          if idx + 1 = move.src then src
          else if idx + 1 = move.dst then dst
          else existing)
    in
    apply_move new_stacks
      { src = move.src; dst = move.dst; count = move.count - 1 }

let apply_move_maintain stacks move =
  let src = Stack.to_list (List.nth_exn stacks (move.src - 1)) in
  let dst = Stack.to_list (List.nth_exn stacks (move.dst - 1)) in
  let chunk, new_src_list = List.split_n src move.count in
  let new_dst = Stack.of_list (List.append chunk dst) in
  let new_src = Stack.of_list new_src_list in
  List.mapi stacks ~f:(fun idx existing ->
      if idx + 1 = move.src then new_src
      else if idx + 1 = move.dst then new_dst
      else existing)

let () =
  let lines = In_channel.read_lines "input/day5.txt" in
  let initial_state, moves = parse_puzzle lines in
  let final_state = List.fold moves ~init:initial_state ~f:apply_move in
  List.iter final_state ~f:(fun x ->
      Out_channel.output_char stdout (Stack.top_exn x));
  Out_channel.newline stdout;

  let initial_state, moves = parse_puzzle lines in
  let final_state =
    List.fold moves ~init:initial_state ~f:apply_move_maintain
  in
  List.iter final_state ~f:(fun x ->
      Out_channel.output_char stdout (Stack.top_exn x));
  Out_channel.newline stdout
