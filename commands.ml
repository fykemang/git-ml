open GitTree
open Unix
open Util

type filename = string
type file_content = string
type file_object = filename * file_content


exception FileNotFound of string 
let init () = begin
  try
    let curr_dir = Unix.getcwd () in
    mkdir ".git-ml" 0o700;
    chdir ".git-ml";
    openfile "HEAD" [O_WRONLY; O_CREAT] 0o666;
    mkdir "objects" 0o700;
    mkdir "info" 0o700;
    mkdir "refs" 0o700;
    mkdir "logs" 0o700;
    mkdir "branches" 0o700;
    chdir "refs";
    mkdir "heads" 0o700;
    mkdir "tags" 0o700;
    chdir "../logs";
    mkdir "refs" 0o700;
    chdir "refs";
    mkdir "heads" 0o700;
    print_endline ("Initialized git-ml repository in " ^ curr_dir);
  with
  | Unix_error (EEXIST, func, file) ->
    print_endline (file ^ " already exists.");
end

(** [read_file ?s file_chnl] reads the [file_chnl] and outputs the content to 
    [s], it closes [file_chnl] after reaching the end of file. *)
let rec read_file ?s:(s = "") file_chnl =
  try
    let cur_line = file_chnl |> input_line in
    read_file file_chnl ~s: (s ^ cur_line ^ "\n")
  with
  | End_of_file -> let _ = file_chnl |> close_in in s

(** [read_dir handle s] reads the directory [dir] and outputs the content to
    [s], it closes [handle] after reaching the end of file. *)
let rec read_dir handle s =
  try
    let cur_file = handle |> readdir in
    let hash = Util.hash_file cur_file in
    if hash = s then cur_file |> open_in |> read_file
    else read_dir handle s 
  with
  | End_of_file -> let _ = handle |> closedir in raise Not_found

let cat s = 
  let fold_header = String.sub s 0 2  in
  let fold_footer = String.sub s 2 (String.length s - 2) in(
    try(
      let ic = open_in (".git-ml/objects/" ^ fold_header ^ "/" ^ fold_footer) in
      let content = read_file ic in
      print_endline content;
    ) with e -> print_endline "Read Issue"
  ) 

(** [cat_string s] is the content of the file at hash_adr s*)
let cat_string s = 
  let fold_header = String.sub s 0 2  in
  let fold_footer = String.sub s 2 (String.length s - 2) in(
    try(
      let ic = open_in (".git-ml/objects/" ^ fold_header ^ "/" ^ fold_footer) in
      let content = read_file ic in
      content;
    ) with e -> raise (FileNotFound 
                         ("file not found: " ^ fold_header ^ "/" ^ fold_footer))
  )

let hash_object file =
  let content = read_file (file |> open_in) in
  Util.print_hash_str ("Blob " ^ content);
  write_hash_contents ("Blob " ^ content) ("Blob " ^ content)

let hash_object_default file =
  let content = read_file (file |> open_in) in
  Util.print_hash_str ("Blob " ^ content)

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
                     ((GitTree.get_subdirectory_tree (subdir) tree) 
                      |> add_file_to_tree_helper t content) tree   
  in
  add_file_to_tree_helper (String.split_on_char '/' name) content tree 

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
let hash_of_git_object = function
  | Tree_Object s -> failwith "Invalid use of function, use hash_of_tree"
  | Blob s -> hash_str ("Blob " ^ s)
  | File s -> hash_str ("File " ^ s)
  | Commit s -> hash_str ("Commit " ^ s)
  | Ref s -> hash_str ("Ref " ^ s)

(** [last_commit_hash ic_ref] gives the last commit for a given in_channel 
    [ic_ref] to a valid ref log file. 
    Requires: [ic_ref] is an in_channel to a valid ref log file**)
let last_commit_hash (ic_ref:in_channel) =
  let rec last_line = function
    | [] -> ""
    | h::[] -> h
    | h::t -> last_line t
  in 
  let commit_string_list = (
    really_input_string ic_ref (in_channel_length ic_ref) |>
    String.split_on_char '\n' |> last_line |> String.split_on_char ' ' ) in
  List.nth commit_string_list 1

let commit 
    (message:string) 
    (branch:string) 
    (file_list:file_object list) : unit = 
  let tree = file_list_to_tree file_list in
  let commit_string =  "Tree_Object " ^ (GitTree.hash_of_tree (tree))
                       ^ "\n" ^ "author Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ "\n" ^ 
                       "commiter Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ "\n\n" ^ 
                       message in
  write_hash_contents commit_string commit_string;
  let oc = open_out (".git-ml/refs/heads/" ^ branch)  in 
  output_string oc (hash_str commit_string);
  let oc_HEAD = open_out (".git-ml/HEAD") in
  output_string oc_HEAD ("refs/heads/" ^ branch);
  try 
    let in_ref = open_in (".git-ml/logs/refs/heads/" ^ branch) in  
    let last_hash = last_commit_hash in_ref in 
    let oc_ref = open_out_gen 
        [Open_append] 0o666 (".git-ml/logs/refs/heads/" ^ branch) in
    output_string oc_ref ("\n" ^ last_hash ^ " " ^ (hash_str commit_string) ^ " " ^ 
                          "Root Author <root@3110.org> " ^ "commit: " 
                          ^ message);
    GitTree.hash_file_subtree tree
  with e -> (
      let last_hash = "00000000000000000000000000000000" in 
      let oc_ref = open_out (".git-ml/logs/refs/heads/" ^ branch) in
      output_string oc_ref (last_hash ^ " " ^ (hash_str commit_string) ^ " " ^ 
                            "Root Author <root@3110.org> " ^ "commit (inital) : " 
                            ^ message););
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

(** [tree_hash_to_git_tree hash_adr] is the GitTree.t of the Tree_Object at
    hash_adr. 
    Requires: [hash_adr] is a valid hash adress to a Tree_Object*)
let rec tree_hash_to_git_tree hash_adr =
  let rec helper (lst:string list) acc =
    match lst with 
    | [] -> ()
    | h::t -> failwith "unimplemented"
  in
  cat_string hash_adr |>
  String.split_on_char '\n' |> failwith "unimplemnetd"

let current_head_to_git_tree s =
  let commit_path = input_line (open_in ".git-ml/HEAD") in
  if (not (Sys.file_exists commit_path)) 
  then raise (FileNotFound ("No such file: " ^ commit_path))
  else (
    let commit_hash = input_line (open_in commit_path) in
    cat_string commit_hash |> String.split_on_char '\n' |>
    List.hd |> String.split_on_char ' ' |> List.tl |> List.hd |>
    failwith "unimplmeneted"
  )

let add (file : string) : unit = try
    chdir ".git-ml";
    let index_out_ch = open_out_gen [Open_creat; Open_append] 0o700 "index" in
    chdir "../";
    let file_in_ch = file |> open_in in
    let file_content = "Blob " ^ (read_file file_in_ch) in
    let file_content_hash = Util.hash_str file_content in
    write_hash_contents file_content file_content;
    Printf.fprintf index_out_ch "%s %s\n" file file_content_hash;
    close_out index_out_ch;
    close_in file_in_ch;
  with
  | Unix_error (ENOENT, name, ".git-ml") -> 
    print_endline ("fatal: Not a git-ml repository" ^
                   " (or any of the parent directories): .git-ml");

