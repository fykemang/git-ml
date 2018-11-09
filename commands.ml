open GitTree
open Unix
open Util
open Sys

module StrMap = Map.Make(String)
module String = struct
  include String
  let format fmt x = Format.fprintf fmt "%s" x
end
module Diff_eng = Diff_engine.Make(String)

type filename = string
type file_content = string
type file_object = filename * file_content

exception FileNotFound of string 
exception No_Commits

let init () = begin
  try
    let curr_dir = Unix.getcwd () in
    mkdir ".git-ml" 0o700;
    chdir ".git-ml";
    let head_out = open_out_gen [Open_creat; Open_wronly] 0o700 "HEAD" in
    output_string head_out "refs/heads/master";
    close_out head_out;
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

let cat s =
  let fold_header = String.sub s 0 2  in
  let fold_footer = String.sub s 2 (String.length s - 2) in
  try
    let ic = open_in 
        (".git-ml/objects/" ^ fold_header ^ Filename.dir_sep ^ fold_footer) in
    let content = read_file ic in
    print_endline content;
  with Sys_error e -> print_endline "File not found due to invalid hash."  

(** [cat_string s] is the content of the file at hash_adr s*)
let cat_string s = 
  let fold_header = String.sub s 0 2  in
  let fold_footer = String.sub s 2 (String.length s - 2) in
  try
    let ic = open_in
        (".git-ml/objects/" ^ fold_header ^ Filename.dir_sep ^ fold_footer) in
    try
      let content = read_file ic in
      content;
    with e -> failwith ("cat_string read error on .git-ml/objects/" 
                        ^ fold_header ^ "/" ^ fold_footer)
  with e -> raise (FileNotFound
                     ("file not found: " ^ fold_header ^ "/" ^ fold_footer))

let hash_object file =
  let content = read_file (file |> open_in) in
  Util.print_hash_str ("Blob " ^ content);
  write_hash_contents ("Blob " ^ content) ("Blob " ^ content)

let hash_object_default file =
  let content = read_file (file |> open_in) in
  Util.print_hash_str ("Blob " ^ content)

let current_branch () = 
  (read_file (open_in  ".git-ml/HEAD")) |> String.split_on_char '\n' |> List.hd 
  |> String.split_on_char '/'  |> List.rev |> List.hd 

let log () =
  let branch = current_branch () in
  try
    let log_string = read_file (open_in (".git-ml/logs/refs/heads/" ^ branch)) 
    in print_endline ("Git Log: \n" ^ log_string)
  with e -> print_endline
              ("Error: No log file has been generated yet. No commits.")

(** [add_file_to_tree name content tree] adds the file with name [name] and 
    content [content] to tree [tree]. *)
let add_file_to_tree name content (tree : GitTree.t) =
  let rec add_file_to_tree_helper name_lst content (tree : GitTree.t) = 
    match name_lst with
    | [] -> tree
    | h::[] -> add_file_to_tree h content tree
    | dir::t -> add_child_tree (get_subdirectory_tree dir tree
                                |> add_file_to_tree_helper t content) tree   
  in add_file_to_tree_helper (String.split_on_char '/' name) content tree 

(** [file_list_to_tree file_list] is the [GitTree] constructed from 
    [file_list]. *)
let file_list_to_tree ?tree:(tree = GitTree.empty_tree_object) 
    (file_list : string StrMap.t) : GitTree.t =
  StrMap.fold (fun file content acc -> add_file_to_tree file content acc) 
    file_list tree

(** [last_commit_hash ic_ref] gives the last commit for a given in_channel 
    [ic_ref] to a valid ref log file. 
    Requires: [ic_ref] is an in_channel to a valid ref log file**)
let last_commit_hash (ic_ref:in_channel) =
  let rec last_line = function
    | [] -> ""
    | h::[] -> h
    | h::t -> last_line t in 
  let commit_string_list = really_input_string ic_ref (in_channel_length ic_ref) 
                           |> String.split_on_char '\n' |> last_line 
                           |> String.split_on_char ' ' in
  List.nth commit_string_list 1

let commit 
    (message : string)
    (branch : string)
    (file_list : string StrMap.t)
    (start_tree : GitTree.t) : unit = 
  let tree = file_list_to_tree file_list ~tree:start_tree in
  let commit_string =  "Tree_Object " ^ (GitTree.hash_of_tree (tree))
                       ^ "\nauthor Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ 
                       "\ncommitter Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ "\n\n" ^ 
                       message in
  write_hash_contents commit_string commit_string;
  print_endline ("[" ^ branch ^ " " ^ (hash_str commit_string) ^ "]");
  let oc = open_out (".git-ml/refs/heads/" ^ branch)  in 
  output_string oc (hash_str commit_string);
  let oc_HEAD = open_out ".git-ml/HEAD" in
  output_string oc_HEAD ("refs/heads/" ^ branch);
  try
    let in_ref = open_in (".git-ml/logs/refs/heads/" ^ branch) in  
    let last_hash = last_commit_hash in_ref in
    let oc_ref = open_out_gen [Open_append] 0o666
        (".git-ml/logs/refs/heads/" ^ branch) in
    output_string oc_ref ("\n" ^ last_hash ^ " " ^ (hash_str commit_string) 
                          ^ " Root Author <root@3110.org> commit: " ^ message);
    GitTree.hash_file_subtree tree
  with e -> let last_hash = "00000000000000000000000000000000" in 
    let oc_ref = open_out (".git-ml/logs/refs/heads/" ^ branch) in
    output_string oc_ref (last_hash ^ " " ^ (hash_str commit_string) ^
                          " Root Author <root@3110.org> commit: " ^ message);
    GitTree.hash_file_subtree tree

(** [tree_content_to_file_list pointer] is the file list of type 
    [string * string list] that is a list of filenames and file contents. 
    Mutually recurisve with [cat_file_to_git_object s] 
    Requires: 
      pointer is a valid pointer to a tree. *)
let tree_content_to_file_list (pointer : string) =
  failwith "Unimplemented"

(** This may not be at all useful. *)
let cat_file_to_git_object (s : string) =
  match String.split_on_char ' ' s with
  | h::t when h = "Blob" -> Blob (List.fold_left (^) "" t )
  | h::t when h = "Tree_Object" -> Tree_Object (List.fold_left (^) "" t ) 
  | _ -> failwith "Unimplemented"

(** [add_file ?idx file] adds file to ".git-ml/objects/" and is a 
    map with a mapping of [file] to a hash of the file contents *)
let add_file ?idx:(idx = StrMap.empty) (file : string) : StrMap.key StrMap.t = 
  let file_content = file |> open_in |> read_file in
  let file_content_hash = Util.hash_str ("Blob " ^ file_content) in
  let file_name = if Str.first_chars file 2 = "./" 
    then Str.string_after file 2
    else file in
  add_file_to_tree file_name file_content empty |> hash_file_subtree;
  StrMap.update file_name (fun opt_v -> Some file_content_hash) idx

(** [to_base_dir base] checks if the current directory has ".git-ml",
    if it does not then recurse through the file system back to the directory
    where it exists
    Requires: The current directory is a child folder of the directory
          where ".git-ml" exists *)
let rec to_base_dir () : unit =
  try ignore (Sys.is_directory ".git-ml") with 
    Sys_error s -> chdir "../"; to_base_dir ()

(** [hash_dir_files address] takes the address [address] to a directory
    and hashes each file and directory within and writes it to .git-ml/objects
    and .git-ml/index using [add_file] *)
let rec add_dir_files ?idx:(idx = StrMap.empty) (address : string) =
  let rec parse_dir ?idx:(idx = StrMap.empty) (dir : Unix.dir_handle) =
    try
      let n = Unix.readdir dir in
      if Filename.check_suffix n "." || Filename.check_suffix n ".git-ml" 
      then parse_dir dir ~idx:idx
      else
        let path = address ^ Filename.dir_sep ^ n in
        chdir address;
        let is_dir = Sys.is_directory n in
        to_base_dir ();
        let new_idx = if is_dir then add_dir_files path ~idx:idx
          else add_file path ~idx:idx in
        parse_dir dir ~idx:new_idx;
    with End_of_file -> closedir dir; idx in
  address |> opendir |> parse_dir ~idx:idx

(** [wr_to_idx idx] writes a mapping from filenames to
    file hashes [idx] to the index file *)
let wr_to_idx (idx : string StrMap.t) : unit = 
  chdir ".git-ml";
  let out_ch = open_out "index" in
  let idx_str = StrMap.fold
      (fun file hash acc  -> (String.concat " " [file; hash])::acc) idx [] 
                |> List.sort compare |> String.concat "\n" in
  output_string out_ch idx_str;
  close_out out_ch;
  chdir ".."

(** [read_idx ()] is a mapping from each entries filename
    to content hash based on the current index *)
let read_idx () : string StrMap.t =
  chdir ".git-ml";
  let curr_idx = open_in_gen [Open_creat] 0o700 "index" 
                 |> read_file |> String.split_on_char '\n' in
  chdir "..";
  let fold_helper acc s =
    if s <> "" then let entry = String.split_on_char ' ' s in
      StrMap.add (List.nth entry 0) (List.nth entry 1) acc
    else acc in
  if curr_idx = [""] then StrMap.empty
  else List.fold_left fold_helper StrMap.empty curr_idx

let add address =
  try
    is_outside_path address;
    let curr_idx = read_idx () in
    let new_idx = if Sys.is_directory address
      then add_dir_files address ~idx:curr_idx
      else add_file address ~idx:curr_idx in
    wr_to_idx new_idx;
  with
  | Unix_error (ENOENT, name, ".git-ml") ->
    print_endline "fatal: Not a git-ml repository \
                   or any of the parent directories): .git-ml"
  | Sys_error e -> print_endline e

let tag () =
  let handle = ".git-ml/refs/tags" |> opendir in
  let string_to_print = read_dir_filenames handle "" in
  print_endline string_to_print

let tag_assign str = 
  let commit_path = input_line (open_in ".git-ml/HEAD") in
  if not (Sys.file_exists (".git-ml/" ^ commit_path))
  then raise (FileNotFound ("No such file: " ^ commit_path))
  else
    let commit_hash = input_line (open_in (".git-ml/" ^ commit_path)) in 
    try
      if Sys.file_exists (".git-ml/refs/tags/" ^ str) 
      then failwith ("fatal: tag " ^ str ^ " already exists")
      else let oc = open_out (".git-ml/refs/tags/" ^ str) in
        output_string oc commit_hash;
        close_out oc
    with
    | Unix_error _ -> ()

(** [tree_hash_to_git_tree hash_adr] is the GitTree.t of the Tree_Object at
    hash_adr. 
    Requires: [hash_adr] is a valid hash address to a Tree_Object *)
let rec tree_hash_to_git_tree ?name:(name = "") hash_adr =
  let rec helper ?acc:(acc = []) (lst : string list) : GitTree.t list =
    match lst with
    | [] -> acc
    | h::tl -> match String.split_on_char ' ' h with 
      | [] -> failwith "Error, helper operating on empty string"
      | h::t when h = "" -> helper tl ~acc:acc
      | h::t when h = "Tree_Object" ->
        helper tl ~acc:((tree_hash_to_git_tree (List.nth t 0) 
                           ~name:(List.nth t 1))::acc)
      | h::t when h = "File" ->
        helper tl ~acc:(Node (File (List.nth t 1), 
                              helper ((cat_string (List.nth t 0)) 
                                      |> (String.split_on_char '\n')))::acc)
      | h::t when h = "Blob" -> begin
          Node (Blob (String.concat "\n" ((String.concat " " t)::tl)), [])::acc
        end
      | h::t -> failwith ("helper only operates on Tree_Object, File or Blob,\
                           attempting to operate on: " ^ h 
                          ^ "| with rest of string:"
                          ^ (List.fold_left (fun a b -> a ^ " " ^ b) "" t)) in
  let spl = cat_string hash_adr |> String.split_on_char '\n' in
  let children = helper spl in Node (Tree_Object name, children)

let read_head () =
  let in_ch = open_in ".git-ml/HEAD" in
  try
    input_line in_ch
  with End_of_file -> close_in in_ch; ""

let current_head_to_git_tree () =
  let commit_path = read_head () in
  if not (Sys.file_exists (".git-ml/" ^ commit_path))
  then empty
  else begin
    let ic = open_in (".git-ml/" ^ commit_path) in 
    let commit_hash = input_line (ic) in
    close_in ic;
    cat_string commit_hash |> String.split_on_char '\n' |> List.hd 
    |> String.split_on_char ' ' |> List.tl |> List.hd |> tree_hash_to_git_tree
  end

let commit_hash_to_git_tree commit_hash =
  cat_string commit_hash |> String.split_on_char '\n' |> List.hd 
  |> String.split_on_char ' ' |> List.tl |> List.hd |> tree_hash_to_git_tree

(** [idx_to_content ()] is a mapping from file name to file content in
    the repository based on index/git-ml *)
let commit_idx () =
  StrMap.fold (fun file hash acc ->
      StrMap.add file (hash |> cat_string |> remove_object_tag "Blob") acc) 
    (read_idx ()) StrMap.empty

(** [wrking_tree_idx ()]  is a mapping from file name to file content in the
    working directory based on index/git-ml *)
let wrking_tree_idx () =
  StrMap.fold (fun file hash acc ->
      let f_in = open_in file in StrMap.add file (read_file f_in) acc) 
    (read_idx ()) StrMap.empty

let commit_command message =
  let branch = current_branch () in 
  try
    let commit_path = input_line (open_in ".git-ml/HEAD") in
    if not (Sys.file_exists (".git-ml/" ^ commit_path))
    then commit message branch (commit_idx ()) GitTree.empty_tree_object
    else commit message branch (commit_idx ()) (current_head_to_git_tree ())
  with e -> commit message branch (commit_idx ()) GitTree.empty_tree_object

let commit_command_default () = 
  commit_command "no commit message provided" 

let rm address =
  try
    is_outside_path address;
    let curr_idx = read_idx () in
    let file_hash =  "Blob " ^ (address |> open_in |> read_file) |> hash_str in
    let curr_tree = current_head_to_git_tree () in
    if StrMap.find address curr_idx = file_hash && mem_hash file_hash curr_tree 
    then
      begin
        wr_to_idx (StrMap.remove address curr_idx);
        Sys.remove address;
      end
    else
      print_endline
        ("error: the following file has changes staged in the index: " ^ 
         address)
  with
  | Unix_error (ENOENT, name, ".git-ml") ->
    print_endline ("fatal: Not a git-ml repository" ^
                   " (or any of the parent directories): .git-ml")
  | Sys_error e -> print_endline e
  | Not_found -> print_endline ("fatal: " ^ address ^ 
                                " did not match any stored or tracked files.")
open Diff_eng

let rec print_diff_obj_lst lst = match lst with
  | [] -> ()
  | hd::tl -> 
    match hd with
    | Del s -> List.iter (fun str -> Format.printf "- %s\n" str) s;
      print_diff_obj_lst tl
    | Add s ->  List.iter (fun str -> Format.printf "+ %s\n" str) s;
      print_diff_obj_lst tl
    | Eq s -> List.iter (fun str -> Format.printf "= %s\n" str) s;
      print_diff_obj_lst tl

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') -> loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let diff () = try
    let commit_idx = commit_idx () in
    let wrking_idx = wrking_tree_idx () in
    let diff_idx = StrMap.fold (
        fun file content acc ->
          let wrk_dir_content = StrMap.find file wrking_idx in
          if content <> wrk_dir_content
          then
            let content_lines = String.split_on_char '\n' content in
            let wrk_content_lines = String.split_on_char '\n' wrk_dir_content in
            let diff_list = Diff_eng.diff content_lines wrk_content_lines in
            StrMap.add file diff_list acc
          else acc
      ) commit_idx StrMap.empty in
    StrMap.iter (fun file diff ->
        Printf.printf "------------------------------------------------\n";
        Printf.printf "File: %s\n" file;
        Printf.printf "------------------------------------------------\n";
        print_diff_obj_lst diff;
        Printf.printf "------------------------------------------------\n\n"
      ) diff_idx
  with 
  | Unix_error (ENOENT, name, ".git-ml") ->
    print_endline ("fatal: Not a git-ml repository" ^
                   " (or any of the parent directories): .git-ml")
  | Sys_error s -> print_endline s

let rm address =
  try
    is_outside_path address;
    let curr_idx = read_idx () in
    let file_hash =  "Blob " ^ (address |> open_in |> read_file) |> hash_str in
    let curr_tree = current_head_to_git_tree () in
    if StrMap.find address curr_idx = file_hash && mem_hash file_hash curr_tree 
    then
      begin
        wr_to_idx (StrMap.remove address curr_idx);
        Sys.remove address;
      end
    else
      print_endline
        ("error: the following file has changes staged in the index: " ^ address)
  with
  | Unix_error (ENOENT, name, ".git-ml") ->
    print_endline ("fatal: Not a git-ml repository" ^
                   " (or any of the parent directories): .git-ml")
  | Sys_error e -> print_endline e
  | Not_found -> print_endline ("fatal: " ^ address ^ 
                                " did not match any stored or tracked files.")

(** [overwrite_all_firles in subtree path tree] overwrites the repo with 
    the files in tree with [path] corresponding to the folder path 
    that [tree] represents*)
let rec overwrite_all_files_in_subtree (path : string) = function
  | [] -> ()
  | Leaf :: t -> failwith "no leafs!"
  | Node (Tree_Object s, lst) :: t -> begin
      try 
        mkdir (path ^ Filename.dir_sep ^ s) 0o700;
        overwrite_all_files_in_subtree (path ^ Filename.dir_sep ^ s) lst;
        overwrite_all_files_in_subtree path t;
      with e ->
        overwrite_all_files_in_subtree (path ^ Filename.dir_sep ^ s) lst;
        overwrite_all_files_in_subtree path t;
    end
  | Node (File s, lst)::t -> let filename = s in 
    let content = GitTree.string_of_git_object (GitTree.git_object_of_tree 
                                                  (List.hd lst))
    in output_string (open_out (path ^ "/" ^ filename)) content;
    overwrite_all_files_in_subtree path t
  | Node (_, lst)::t -> failwith "invalid object in GitTree"


let checkout_path path = 
  let rec checkout_path_helper
      (subdir_lst : string list) 
      (tree : GitTree.t) : unit =
    match subdir_lst with 
    | [] -> failwith ("checkout path error, trying to checkout invalid path " 
                      ^ path)
    | h::[] -> begin
        let subdir_tree = if h = "." then (tree) 
          else (GitTree.get_subdirectory_tree h tree) in
        match subdir_tree with
        | Leaf -> failwith 
                    "Error GitTree.get_subdirectory_tree giving Leaf output"
        | Node (o, []) -> failwith "Improper file path in checkout"
        | Node (o, lst) -> overwrite_all_files_in_subtree path lst
      end
    | h::t -> let subdir_tree = GitTree.get_subdirectory_tree h tree in 
      checkout_path_helper t subdir_tree
  in
  let current_tree = current_head_to_git_tree () in 
  try
    checkout_path_helper (String.split_on_char '/' path) current_tree
  with e -> raise e

let checkout_branch branch = 
  let current_head_pointer = if Sys.file_exists ".git-ml/HEAD" 
    then input_line (open_in (".git-ml/" ^ (read_head ())))
    else "00000000000000000000000000000000"
  in
  let oc = open_out ".git-ml/HEAD" in
  output_string oc ("refs/heads/" ^ branch);
  close_out oc;
  if not (Sys.file_exists (".git-ml/refs/heads/" ^ branch))
  then 
    begin
      print_endline ("Creating new branch " ^ branch);
      let oc2 = open_out (".git-ml/refs/heads/" ^ branch) in 
      output_string oc2 current_head_pointer;
      close_out oc2;
      if current_head_pointer <> "00000000000000000000000000000000" then 
        let last_hash = "00000000000000000000000000000000" in
        let oc_ref = open_out (".git-ml/logs/refs/heads/" ^ branch) in
        output_string oc_ref ("\n" ^ last_hash ^ " " ^ current_head_pointer
                              ^ " Root Author <root@3110.org> commit: created new branch " 
                              ^ branch);
    end
  else print_endline ("Switching to branch " ^ branch);
  checkout_path "."


(** [ancestor_list branch] is the list of ancestor commits on branch 
    [branch]. *)
let ancestor_list branch =  
  let rec ancestor_list_helper (acc:string list) (line_list:string list) =
    match line_list with 
    | [] -> List.rev acc 
    | h::t -> 
      let hash = (String.split_on_char ' ' h |> List.hd) in 
      ancestor_list_helper (hash::acc) t
  in
  let ic = open_in (".git-ml/logs/refs/heads/" ^ branch) in
  let log_string = read_file ic in
  close_in ic; 
  let line_lst = log_string |> String.split_on_char '\n' |> List.rev in
  ancestor_list_helper ((line_lst |> List.hd |> String.split_on_char ' ' |> 
                         List.tl |> List.hd)::[]) line_lst

(** [first_commit commit_list] is the first nonzero
    commit of a given [commit_list] 
    Requires:
    [commit_lst] is a valid zero terminating commit list, like one given from
    [ancestor_list]. *)
let rec first_commit (commit_lst:string list) =
  match commit_lst with 
  | [] -> failwith "empty commit list error"
  | h::zero::t when zero = "00000000000000000000000000000000" -> h 
  | h::h2::[] -> failwith "error with commit_list in first_commit"
  | h::t -> first_commit t

(** [first_common_commit commit_list target] is the target_hash if the 
    target_hash is in the git_tree *)
let rec first_common_commit commit_list target_hash =
  match commit_list with 
  | [] -> "FAIL"
  | h::t when h = target_hash -> target_hash
  | h::t -> first_common_commit t target_hash

(** [common_ancestor_trivial branch_a branch_b] is the last common ancesotor
    between branch_a and branch_b 
    Requires:
      branch_b originated from branch_a or vice versa*)
let common_ancestor_trivial branch_a branch_b : string = 
  let anc_lst_a = ancestor_list branch_a in
  let anc_lst_b = ancestor_list branch_b in 
  let possible_ancestor = first_common_commit anc_lst_b 
      (first_commit anc_lst_a) in (* this case is when a was branched off of b *)
  if possible_ancestor = "FAIL" then (first_common_commit anc_lst_a 
                                        (first_commit anc_lst_b))
  else possible_ancestor

(** [merge_base branches] gives the first common ancesstor between [branches]
    Requires:
    branches is of the form ["branch_a branch_b"] *)
let merge_base branches = 
  match String.split_on_char ' ' branches with 
  | branch_a::branch_b::[] -> print_endline (common_ancestor_trivial branch_a 
                                               branch_b)
  | _ -> failwith "Invalid use of command merge-base"

(** [merge_branch branch_a branch_b] merges branch_a onto branch_b
    Requires:
    current working branch is [branch_b]
    [branch_a] is not the master branch*)
let merge_branch branch_a branch_b = 
  let rec merge_tree_list lst_a hash_a lst_b hash_b path =
    match lst_a with 
    | [] -> ()
    | h::t when 
        ((not (GitTree.git_object_in_tree_list 
                 (GitTree.git_object_of_tree h) lst_b)))-> 
      overwrite_all_files_in_subtree path (h::[]);
      (merge_tree_list t hash_a lst_b hash_b path)
    | Node (File s, (Node (Blob content_a, []))::[])::t -> begin 
        let matching_tree_b = 
          (GitTree.matching_tree_in_tree_lst (File s) lst_b) in 
        match matching_tree_b with 
        | Node (File s_b, (Node (Blob content_b, []))::[]) -> begin 
            let write_string = if content_a = content_b then content_a
              else (print_endline ("ERROR: merge conflict in " ^ s_b); 
                    ">>>>>>> " ^ "branch: " ^ branch_a ^ "| commit: " ^ 
                    hash_a ^ "\n" ^ content_a ^ 
                    "\n" ^ "<<<<<<< " ^ "branch: " ^ branch_b ^ "| commit: " 
                    ^ hash_b ^ "\n" 
                    ^ content_b ^ "\n" ^ ">>>>>>>") in 
            let oc = open_out (path ^ Filename.dir_sep ^ s) in
            output_string oc write_string;
            close_out oc;
            (merge_tree_list t hash_a lst_b hash_b path)
          end
        | _ -> failwith "issue in matching_tree_in_tree_lst"
      end
    | Node (Tree_Object s, sublst_a)::t -> begin
        let matching_tree_b = 
          (GitTree.matching_tree_in_tree_lst (Tree_Object s) lst_b) in 
        match matching_tree_b with 
        | Node (Tree_Object s_b, sublst_b) -> 
          (merge_tree_list sublst_a hash_a sublst_b 
             hash_b (path ^ Filename.dir_sep ^ s));
          (merge_tree_list t hash_a lst_b hash_b path) 
        | _ -> 
          failwith 
            "literarly impossible if matching_tree_in_tree_lst works correctly"
      end
    | _ -> failwith "invalid GitObjects in git_tree merge_tree_list"
  in
  let com_anc_hash = common_ancestor_trivial branch_a branch_b in
  if com_anc_hash = "FAIL" 
  then print_endline 
      "Cannot merge a branch onto a branch it didn't originate from!"
  else 
    try 
      let hash_a = List.hd (ancestor_list branch_a) in
      let hash_b = List.hd (ancestor_list branch_b) in
      let tree_a = commit_hash_to_git_tree hash_a in 
      let tree_b = commit_hash_to_git_tree hash_b in
      let tree_com = commit_hash_to_git_tree (com_anc_hash) in
      if branch_a = "master" then print_endline "ERROR: Cannot merge from master"
      else 
      if branch_b <> current_branch () 
      then print_endline "ERROR: Must be on branch that you are merging to."
      else 
        match tree_a, tree_b with 
        | Node (Tree_Object _, lst_a), Node (Tree_Object _, lst_b) -> 
          merge_tree_list lst_a hash_a lst_b hash_b ".";
          print_endline "--Merge Completed--";
          print_endline 
            "Fix any merge conflicts, and stage and commit your changes to finalize the merge"
        | _ -> failwith "Invalid form on GitTree from branch a or b"
    with e -> print_endline "Issue with merge, make sure all changes are staged 
    and commited on the current branch before you attempt a merge."
let merge_branch_command branches = 
  match String.split_on_char ' ' branches with 
  | branch_a::branch_b::[] -> merge_branch branch_a branch_b
  | _ -> failwith "Invalid use of command merge-base"





(*------------------------------status code ---------------------------------*)
(** compare_files *)


(** [compare_files blob_obj file_name] is true if the file with name[file_name]
    has the same content as in [blob_obj]. *)
let compare_files blob_obj file_name = 
  let content = read_file (file_name |> open_in) in
  hash_str "Blob " ^ content = hash_str "Blob " ^ blob_obj

(** [compare_file_blob prefix f l acc] is the updated [acc'] after 
    comparing [l]. *)
let rec compare_file_blob prefix f l acc = 
  match l with
  | [Node (Blob b, l')] -> 
    let add = if prefix = "" then f else  (prefix ^ "/" ^ f) in
    if compare_files b add then acc else f::acc
  | _ -> failwith "A file must have one and only one blob child"

(** [status2_help address acc tree] is the updated [acc'] after processing 
    [tree]. *)
let rec status2_help address acc tree = 
  (** [status2_help_children acc' l] is the updated [acc'] after processing 
      all treenodes in [l]. *)
  let rec status2_help_children 
      (level_addr : string) 
      (acc': string list) 
      (l: GitTree.t list) : string list = 
    match l with
    | [] -> acc'
    | Leaf::t-> failwith "there should be no leaf"
    | Node (o, lst)::t ->   
      let updated = status2_help level_addr acc' (Node (o, lst)) in
      status2_help_children level_addr (updated @ acc') t 
  in
  match tree with
  | Leaf -> failwith "there should be no Leaf in the tree"
  | Node (Tree_Object treeob, l) -> 
    let add = if address = "" then treeob else (address ^ "/" ^ treeob) in
    status2_help_children add acc l
  | Node (File f, l) -> compare_file_blob address f l acc
  | Node (Blob b, l) -> failwith "cannot reach any blob"
  | Node (Commit c, l) -> failwith "cannot reach any commit"
  | Node (Ref r, l) -> failwith "cannot reach any ref"

(** [status2 ()] are the files that are different between working directory 
    (tree) and current commit: paths that have differences between the working 
    tree and the index file. *)
let status2 () = status2_help "" [] (current_head_to_git_tree ())

(** [get_file's_blob_hash lst] is the hash of the blob represented in [lst]. *)
let get_file's_blob_hash = function
  | [ Node (Blob b, l') ] -> b
  | _ -> failwith "A file must have one and only one blob child"

(** [find_file address filename acc tree] is the content of [filename] in 
    [tree]. *)
let rec find_file address filename acc tree : string = 
  let rec find_help_children
      (filename: string)
      (level_addr : string) 
      (acc' : string)
      (l: GitTree.t list) : string = 
    match l with
    | [] -> acc'
    | Leaf::t-> failwith "there should be no leaf in the tree"
    | Node (o, lst) as node :: t -> 
      let potential_hash = find_file level_addr filename acc' node in 
      find_help_children filename level_addr (acc' ^ potential_hash) t
  in
  match tree with
  | Leaf -> failwith "there should be no Leaf in the tree"
  | Node (Tree_Object treeob, l) -> 
    let add = if address = "" then treeob else (address ^ "/" ^ treeob) in
    find_help_children filename add acc l
  | Node (File f, l) -> 
    let add = if address = "" then f else address ^ "/" ^ f in
    if add = filename then (get_file's_blob_hash l) else ""
  | Node (Blob b, l) -> failwith "cannot reach any blob"
  | Node (Commit c, l) -> failwith "cannot reach any commit"
  | Node (Ref r, l) -> failwith "cannot reach any ref"

(** [file_changed tree filename hash] is true if the content of [filename] 
    has changed from [hash]. *)
let file_changed (tree: GitTree.t) (filename : string) (hash : string) : bool = 
  let hash_in_head = find_file "" filename "" tree in 
  hash <> hash_str ("Blob " ^ hash_in_head)

(** [status1] returns all the files that have been added but not yet commited. 
    It returns a list of files that are in index but not in the current HEAD 
    commit. *)
let status1 () : string list = 
  let idx = read_idx () in 
  let updated_map = 
    StrMap.filter (current_head_to_git_tree () |> file_changed) idx in 
  let bindings = StrMap.bindings updated_map in
  List.split bindings |> fst

(** [untracked filename] returns true if the [filename]'s content has been 
    changed since the last commit. *)
let untracked filename = 
  (find_file "" filename "" (current_head_to_git_tree ())) = ""

(** [read_dir dir prefix] reads the [dir] and returns all files that are 
    untracked [prefix] is the accumulator for the file path. *)
let rec read_dir (dir : string) (prefix: string) =
  (* prefix + filename = the whole path. *)
  let rec read_dir_help 
      (acc: string list) 
      (handle : Unix.dir_handle)
      (prefix: string) =
    try
      let n = Unix.readdir handle in
      if n = "." || n = ".." || n = ".git-ml" || n = ".DS_Store" || n = ".git"
      then read_dir_help acc handle prefix
      else if not (is_directory (prefix ^ n)) then
        let res_lst = (if n |> untracked then (prefix ^ n)::acc else acc) in 
        read_dir_help res_lst handle prefix
      else
        let new_lst = read_dir (prefix ^ n) (prefix ^ n ^ "/") in
        read_dir_help (new_lst @ acc) handle prefix
    with End_of_file -> closedir handle; acc in
  read_dir_help [] (dir |> opendir) prefix

(** [status3] traverses the working directory and list all the files that are 
    untracked comparing with the last commit. *)
let status3 () = read_dir "." ""

(** [invoke_status status msg] is a helper for returning the message for each 
    [status] condition, and [msg] is the message for that condition. *)
let invoke_status status msg = 
  let lst = status () |> List.sort_uniq (String.compare) in 
  if List.length lst > 0 then
    print_endline (msg ^ " \n");
  List.iter (fun x -> print_endline x) lst

let status () = 
  let cur_tree = current_head_to_git_tree () in 
  if cur_tree = GitTree.empty 
  then print_endline 
      "There is no commit yet, you can first add files and then commit."
  else 
    invoke_status status1 
      "The following files are about to be commited:";
  invoke_status status2 
    "The following files have been modified since the last commit:";
  invoke_status status3 
    "The following files are untracked:"
