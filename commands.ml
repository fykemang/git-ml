open GitTree
open Unix
open Util

type filename = string
type file_content = string
type file_object = filename * file_content

let init () = begin
  try
    let curr_dir = Unix.getcwd () in
    mkdir ".git-ml" 0o700;
    chdir ".git-ml";
    openfile "index" [O_WRONLY; O_CREAT] 0o666;
    openfile "HEAD" [O_WRONLY; O_CREAT] 0o666;
    mkdir "objects" 0o700;
    mkdir "info" 0o700;
    mkdir "refs" 0o700;
    mkdir "branches" 0o700;
    print_endline (getcwd ());
    print_endline ("Initialized git-ml repository in " ^ curr_dir);
  with
  | Unix_error (EEXIST, func, file) ->
    print_endline (file ^ " already exists.");
end

let print_hash s = print_endline (Util.hash_str s)

let save_hash s = failwith "Unimplemented"

let read_file file = 

  let rec read_dir handle s = 
    try let cur_file = handle |> readdir in
      let hash = Util.hash_file cur_file in
      if hash=s then read_file cur_file 
      else read_dir handle s with
    | End_of_file -> let _ = handle |> closedir in raise Not_found

let cat s = 
  let handle = opendir ".git-ml/objects" in
  read_dir handle s

let ls_tree s = failwith "Unimplemented"

let log s = failwith "Unimplemented"

let add_file_to_tree name content (tree:GitTree.t) =
  let rec add_file_to_tree_helper name_lst content (tree:GitTree.t) = 
    match name_lst with 
    |[] -> tree 
    |h::[] -> (GitTree.add_file h content tree)
    |subdir::t -> GitTree.add_child_tree 
                    ((GitTree.get_subdirectory_tree subdir tree) |> 
                     add_file_to_tree_helper t content) tree   
  in 
  add_file_to_tree_helper (String.split_on_char '/' name) content tree 

let file_list_to_tree (file_list : file_object list) =
  let rec helper acc (lst : file_object list) = 
    match lst with 
    |[] -> acc
    |(file_name,file_content)::t -> 
      helper (add_file_to_tree file_name file_content acc) t
  in helper GitTree.empty_tree_object file_list
