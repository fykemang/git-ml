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
    openfile "HEAD" [O_WRONLY; O_CREAT] 0o666;
    mkdir "objects" 0o700;
    mkdir "info" 0o700;
    mkdir "refs" 0o700;
    mkdir "branches" 0o700;
    chdir "refs";
    mkdir "heads" 0o700;
    mkdir "tags" 0o700;
    print_endline ("Initialized git-ml repository in " ^ curr_dir);
  with
  | Unix_error (EEXIST, func, file) ->
    print_endline (file ^ " already exists.");
end

let print_hash_file f = print_endline (Util.hash_file f)

let print_hash_str s = print_endline (Util.hash_str s)

let print_hash s = print_endline (Util.hash_str s)

(* let save_hash s = failwith "Unimplemented" *)

(** [read_file file_chnl s] reads the [file_chnl] and outputs the content to [s],
    it closes [file_chnl] after reaching the end of file. *)
let rec read_file file_chnl s = 
  try let cur_line = file_chnl |> input_line in
    let s = s ^ cur_line ^ "\n" in
    read_file file_chnl s with
  | End_of_file -> let _ = file_chnl |> close_in in s

(** [read_dir handle s] reads the directory [dir] and outputs the content to
    [s], it closes [handle] after reaching the end of file. *)
let rec read_dir handle s = 
  try let cur_file = handle |> readdir in
    let hash = Util.hash_file cur_file in
    if hash = s then read_file (open_in cur_file) ""
    else read_dir handle s with
  | End_of_file -> let _ = handle |> closedir in raise Not_found

let cat s = 
  let fold_header = String.sub s 0 2  in
  let fold_footer = String.sub s 2 (String.length s - 2) in(
    try(
      let ic = open_in (
          ".git-ml/objects/" ^ fold_header ^ "/" ^ fold_footer) in
      let content = (read_file ic "") in
      (print_endline content);
    ) with e -> print_endline "Read Issue"
  ) 

(*let hash_object file = 
  let content = read_file (file |> open_in) "" in
  let out_chnl = open_out file in
  let _ = output_string out_chnl content in 
  let _ = out_chnl |> close_out 
  in hash_file file*)

let hash_object file =
  let content = read_file (file |> open_in) "" in
  let () = print_hash_str ("Blob "^content) in
  write_hash_contents ("Blob "^content) ("Blob "^content)

let hash_object_default file =
  let content = read_file (file |> open_in) "" in
  print_hash_str ("Blob "^content)

let ls_tree s = failwith "Unimplemented"

let log s = failwith "Unimplemented"

(** [add_file_to_tree name content tree] adds the file with name [name] and 
    content [content] to tree [tree]. *)
let add_file_to_tree name content (tree : GitTree.t) =
  let rec add_file_to_tree_helper name_lst content (tree : GitTree.t) = 
    match name_lst with 
    | [] -> tree
    | h::[] -> GitTree.add_file h content tree
    | subdir::t -> GitTree.add_child_tree 
                     ((GitTree.get_subdirectory_tree (subdir) tree) |> 
                      add_file_to_tree_helper t content) tree   
  in
  (**let add_file_to_tree_helper name_lst content (tree:GitTree.t) = 
     match name_lst with
     | [] -> tree
     | h::t -> let subdir = List.fold_right 
                  (fun (a) (b:string) -> (a ^ "/" ^ b)) (List.rev t) "" in 
      GitTree.get_subdirectory_tree subdir tree |>
      GitTree.add_file h content
     in*)
  add_file_to_tree_helper 
    (String.split_on_char '/' name) content tree 

(** [file_list_to_tree file_list] is the [GitTree] constructed from 
    [file_list]. *)
let file_list_to_tree (file_list : file_object list) =
  let rec helper acc (lst : file_object list) = 
    match lst with 
    | [] -> acc
    | (file_name,file_content)::t -> 
      helper (add_file_to_tree file_name file_content acc) t
  in helper GitTree.empty_tree_object file_list

(** [hash_of_git_object obj] is the string hash of a given [git_object] obj *)
let hash_of_git_object (obj : git_object) : string = 
  match obj with
  | Tree_Object s -> failwith "Invalid use of function, use hash_of_tree"
  | Blob s -> hash_str ("Blob " ^ s)
  | File s -> hash_str ("File " ^ s)
  | Commit s -> hash_str ("Commit " ^ s)
  | Ref s -> hash_str ("Ref " ^ s)

let commit 
    (message:string) 
    (branch:string) 
    (file_list:file_object list) : unit = 
  let tree = file_list_to_tree file_list in 
  let oc = open_out (".git-ml/refs/heads/" ^ branch) in 
  output_string oc ("Tree_Object " ^ (GitTree.hash_of_tree (tree))
                    ^ "\n");
  output_string oc ("author Root Author <root@3110.org> " ^
                    (hash_str "root@3110.org")^"\n");
  output_string oc ("commiter Root Author <root@3110.org> " ^
                    (hash_str "root@3110.org") ^ "\n");
  GitTree.hash_file_subtree tree

(** [tree_content_to_file_list pointer] is the file list of type 
    [string * string list] that is a list of filenames and file contents. 
    Mutually recurisve with [cat_file_to_git_object s] 
    Requires: 
      pointer is a valid pointer to a tree. *)
let tree_content_to_file_list (pointer:string) =
  failwith "Unimplemented"

(** This may not be at all useful. *)
let cat_file_to_git_object (s:string) =
  match String.split_on_char ' ' s with
  | h::t when h = "Blob" -> Blob (List.fold_left (^) "" t )
  | h::t when h = "Tree_Object" -> Tree_Object (List.fold_left (^) "" t ) 
  | _ -> failwith "Unimplemented"

let add (file : string) : unit = try
    chdir ".git-ml";
    let file_index = openfile "index" [O_APPEND; O_CREAT; O_RDWR] 0o700 in
    let bytes_written = write_substring file_index file 0 0 in
    print_endline (string_of_int bytes_written);
    close file_index;
  with
  | Unix_error (e, name, param) -> print_endline name