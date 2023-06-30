open Core

let group_left groups item =
  if String.is_empty item then [] :: groups
  else
    match groups with
    | [] -> [ [ int_of_string item ] ]
    | g :: rest -> (int_of_string item :: g) :: rest

let get_elves file_path =
  let input = In_channel.read_all file_path in
  let lines = String.split_lines input in
  List.fold_left lines ~init:[] ~f:group_left

(*let print_list l =
  print_string "[";
  List.iter l ~f:(fun item -> print_string (string_of_int item ^ ","); );
  print_endline "],";;
*)

(*let print_ll ll =
  print_string "[";
  List.iter ll ~f:(fun item -> print_list item);
  print_endline "]";;
*)

let () =
  (* Load the input file *)
  let file_path = "input/day1.txt" in
  let elves = get_elves file_path in
  let elf_calorie_sum =
    List.map elves ~f:(fun elf -> List.reduce_exn elf ~f:( + ))
  in
  let max_calorie_elf =
    Option.value_exn (List.max_elt elf_calorie_sum ~compare:( - ))
  in
  print_endline (string_of_int max_calorie_elf);
  let sorted_elves = List.sort elf_calorie_sum ~compare:( fun a b -> b - a ) in
  let elf_sums =
    List.foldi sorted_elves ~init:0
      ~f:(fun idx accum item ->
        accum + if idx > 2 then 0 else item)
  in
  print_endline (string_of_int elf_sums)
