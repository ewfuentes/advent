open Core

let find_duplicate line =
  let half_size = String.length line / 2 in
  let first, last =
    (String.slice line 0 half_size, String.slice line half_size 0)
  in
  let duplicate_index =
    String.to_list first
    |> List.map ~f:(fun x -> String.index last x)
    |> List.reduce_exn ~f:Option.first_some
    |> Option.value_exn
  in
  String.get last duplicate_index

let score_char char =
  if Char.is_lowercase char then Char.to_int char - Char.to_int 'a' + 1
  else Char.to_int char - Char.to_int 'A' + 27

let rec chunk_lines ?(chunks : (string * string * string) list = []) lines =
  match lines with
  | [] -> chunks
  | a :: b :: c :: rest -> chunk_lines rest ~chunks:((a, b, c) :: chunks)
  | _ -> failwith "Number of items not divisible by three"

let find_common_item (a, b, c) =
  let idxs =
    String.to_list a
    |> List.map ~f:(fun char ->
           Option.both (String.index b char) (String.index c char))
    |> List.reduce_exn ~f:Option.first_some
  in
  let b_idx, _ = Option.value_exn idxs in
  String.get b b_idx

let () =
  let lines = In_channel.read_lines "input/day3.txt" in
  lines |> List.map ~f:find_duplicate |> List.map ~f:score_char
  |> List.reduce ~f:( + ) |> Option.value_exn |> Int.to_string |> print_endline;

  chunk_lines lines
  |> List.map ~f:find_common_item
  |> List.map ~f:score_char |> List.reduce_exn ~f:( + ) |> Int.to_string
  |> print_endline
