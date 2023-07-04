open Core

type file_node = { name : string; size : int }

type dir_node = {
  name : string;
  parent : dir_node option;
  mutable files : file_node list;
  mutable dirs : dir_node list;
}

let node_to_string node =
  "<node: " ^ node.name ^ " files: ["
  ^ List.fold node.files ~init:"" ~f:(fun accum f ->
        (match accum with "" -> "" | _ -> accum ^ ", ")
        ^ "(" ^ f.name ^ ", " ^ Int.to_string f.size ^ ")")
  ^ "] dirs: ["
  ^ List.fold node.dirs ~init:"" ~f:(fun accum d ->
        match accum with "" -> d.name | _ -> accum ^ ", " ^ d.name)
  ^ "] parent: "
  ^ (match node.parent with None -> "None" | Some p -> p.name)
  ^ ">"

let parse_command (pwd : dir_node) line : dir_node =
  match String.split line ~on:' ' with
  | [ "$"; "cd"; ".." ] -> Option.value_exn pwd.parent
  | [ "$"; "cd"; "/" ] -> pwd
  | [ "$"; "cd"; dir_name ] ->
      List.find_exn pwd.dirs ~f:(fun dir -> String.equal dir.name dir_name)
  | _ -> pwd

let parse_file (pwd : dir_node) line : dir_node =
  let parts = String.split line ~on:' ' in
  let size = Int.of_string (List.nth_exn parts 0) in
  let file_name = List.nth_exn parts 1 in
  pwd.files <- { name = file_name; size } :: pwd.files;
  pwd

let parse_directory (pwd : dir_node) line : dir_node =
  match String.split ~on:' ' line with
  | [ "dir"; dir_name ] ->
      pwd.dirs <-
        { name = dir_name; files = []; dirs = []; parent = Some pwd }
        :: pwd.dirs;
      pwd
  | _ -> failwith ("Can't parse " ^ line)

let parse_line (pwd : dir_node) line : dir_node =
  let c = String.get line 0 in
  if Char.equal c '$' then parse_command pwd line
  else if Char.is_digit c then parse_file pwd line
  else parse_directory pwd line

let parse_tree lines : dir_node =
  let root_node = { name = "/"; parent = None; files = []; dirs = [] } in
  let pwd = ref root_node in
  List.iter lines ~f:(fun line -> pwd := parse_line !pwd line);
  root_node

let rec repeat_str ?(in_progress : string = "") str count =
  if count = 0 then in_progress
  else repeat_str str (count - 1) ~in_progress:(str ^ in_progress)

let rec print_file_tree ?(depth : int = 0) node =
  print_endline (repeat_str "  " depth ^ node.name);
  List.iter node.files ~f:(fun file ->
      print_endline
        (repeat_str "  " (depth + 1) ^ file.name ^ " " ^ Int.to_string file.size));
  List.iter node.dirs ~f:(fun dir -> print_file_tree ~depth:(depth + 1) dir)

let key_of_node node =
  let rec helper str node =
    let new_str = node.name ^ str in
    match node.parent with None -> new_str | Some p -> helper new_str p
  in
  helper "" node

let rec compute_directory_sizes ?sizes node =
  print_endline ("Computing size for " ^ node_to_string node);
  let init_sizes =
    match sizes with
    | None -> Hashtbl.create (module String)
    | Some table -> table
  in
  if Option.is_some (Hashtbl.find init_sizes (key_of_node node)) then (
    print_endline "Already Computed!";
    init_sizes)
  else
    let new_sizes =
      List.fold node.dirs ~init:init_sizes ~f:(fun accum dir ->
          compute_directory_sizes ~sizes:accum dir)
    in
    let immediate_file_sizes =
      List.fold node.files ~init:0 ~f:(fun accum file -> accum + file.size)
    in
    let immediate_folder_sizes =
      List.fold node.dirs ~init:0 ~f:(fun accum dir ->
          accum + Hashtbl.find_exn new_sizes (key_of_node dir))
    in
    let folder_size = immediate_file_sizes + immediate_folder_sizes in
    Hashtbl.add_exn new_sizes ~key:(key_of_node node) ~data:folder_size;
    print_endline ("Size for " ^ node.name ^ " " ^ Int.to_string folder_size);
    new_sizes

let () =
  let lines = In_channel.read_lines "input/day7.txt" in
  let file_tree = parse_tree lines in
  let sizes = compute_directory_sizes file_tree in
  let small_folders_size =
    Hashtbl.fold sizes ~init:0 ~f:(fun ~key ~data accum ->
        if data <= 100000 then accum + data else accum)
  in
  print_endline (Int.to_string small_folders_size);
  let free_space = 70000000 - Hashtbl.find_exn sizes "/" in
  let to_delete = 30000000 - free_space in
  let large_enough_dirs =
    Hashtbl.filter sizes ~f:(fun folder_size -> folder_size > to_delete)
  in
  let folder_list = Hashtbl.to_alist large_enough_dirs in
  let value = List.min_elt folder_list ~compare:(fun (_, a) (_, b) -> a - b) in
  let _, to_delete = Option.value_exn value in
  print_endline (Int.to_string to_delete)
