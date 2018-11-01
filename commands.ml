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
    ignore (openfile "HEAD" [O_WRONLY; O_CREAT] 0o666);
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

(** [read_dir handle s] reads the directory [dir] and outputs the filenames to
    [s], it closes [handle] after reaching the end of file. *)
let rec read_dir_filenames handle s =
  try
    let cur_file = handle |> readdir in
    if cur_file="." || cur_file=".." then read_dir_filenames handle s else
      read_dir_filenames handle (s^cur_file^"\n")
  with
  | End_of_file -> let _ = handle |> closedir in s

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
      try (let content = read_file ic in
           content;) with e -> (
          failwith 
            ("cat_string read error on" ^ 
             ".git-ml/objects/" ^ fold_header ^ "/" ^ fold_footer))
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

let log branch = 
  try
    let log_string = read_file (open_in (".git-ml/logs/refs/heads/" ^ branch)) 
    in
    print_endline ("Git Log: \n" ^ log_string)
  with e -> print_endline ("Error finding log file")

(** [add_file_to_tree name content tree] adds the file with name [name] and 
    content [content] to tree [tree]. *)
let add_file_to_tree name content (tree : GitTree.t) =
  let rec add_file_to_tree_helper name_lst content (tree : GitTree.t) = 
    match name_lst with 
    | [] -> tree
    | h::[] -> GitTree.add_file_to_tree h content tree
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

let file_list_to_tree_non_empty (file_list : file_object list) (start_tree) =
  let rec helper acc (lst : file_object list) = 
    match lst with 
    | [] -> acc
    | (file_name,file_content)::t -> 
      helper (add_file_to_tree file_name file_content acc) t
  in helper start_tree file_list

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
    (file_list:file_object list) 
    (start_tree:GitTree.t) : unit = 
  let tree = file_list_to_tree_non_empty file_list start_tree in
  let commit_string =  "Tree_Object " ^ (GitTree.hash_of_tree (tree))
                       ^ "\n" ^ "author Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ "\n" ^ 
                       "commiter Root Author <root@3110.org> " ^
                       (hash_str "root@3110.org") ^ "\n\n" ^ 
                       message in
  write_hash_contents commit_string commit_string;
  print_endline ("[" ^ branch ^" " ^(hash_str commit_string) ^ "]");
  let oc = open_out (".git-ml/refs/heads/" ^ branch)  in 
  output_string oc (hash_str commit_string);
  let oc_HEAD = open_out (".git-ml/HEAD") in
  output_string oc_HEAD ("refs/heads/" ^ branch);
  try 
    let in_ref = open_in (".git-ml/logs/refs/heads/" ^ branch) in  
    let last_hash = last_commit_hash in_ref in 
    let oc_ref = open_out_gen 
        [Open_append] 0o666 (".git-ml/logs/refs/heads/" ^ branch) in
    output_string oc_ref ("\n" ^ last_hash ^ " " ^ (hash_str commit_string) 
                          ^ " " ^ 
                          "Root Author <root@3110.org> " ^ "commit: " 
                          ^ message);
    GitTree.hash_file_subtree tree
  with e -> (
      let last_hash = "00000000000000000000000000000000" in 
      let oc_ref = open_out (".git-ml/logs/refs/heads/" ^ branch) in
      output_string oc_ref (last_hash ^ " " ^ (hash_str commit_string) ^ " " ^ 
                            "Root Author <root@3110.org> " 
                            ^ "commit (inital) : " 
                            ^ message);
      GitTree.hash_file_subtree tree)

(** [tree_content_to_file_list pointer] is the file list of type 
    [string * string list] that is a list of filenames and file contents. 
    Mutually recurisve with [cat_file_to_git_object s] 
    Requires: 
      pointer is a valid pointer to a tree. *)
let tree_content_to_file_list (pointer:string) =
  failwith "Unimplemented"

(** This may not be at all useful. *)
let cat_file_to_git_object (s : string) =
  match String.split_on_char ' ' s with
  | h::t when h = "Blob" -> Blob (List.fold_left (^) "" t )
  | h::t when h = "Tree_Object" -> Tree_Object (List.fold_left (^) "" t ) 
  | _ -> failwith "Unimplemented"

(** [add_file file] writes the hash, name, and time-stamp of the file [file]  
    to .git-ml/index and stores the file content in .git-ml/objects *)
let add_file (file : string) : unit =
  chdir ".git-ml";
  let index_out_ch = open_out_gen [Open_creat; Open_append] 0o700 "index" in
  chdir "../";
  let file_in_ch = file |> open_in in
  let file_content = read_file file_in_ch in
  let file_content_hash = Util.hash_str ("Blob " ^ file_content) in
  add_file_to_tree file file_content empty |> hash_file_subtree;
  Printf.fprintf index_out_ch "%s %s\n" file file_content_hash;
  close_out index_out_ch;
  close_in file_in_ch

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
let rec add_dir_files (address : string) : unit =
  let rec parse_dir dir = 
    try
      let n = readdir dir in
      if Filename.check_suffix n "." || Filename.check_suffix n ".git-ml" 
      then parse_dir dir
      else
        let path = address ^ Filename.dir_sep ^ n in
        chdir address;
        let is_dir = Sys.is_directory n in
        to_base_dir ();
        if is_dir then add_dir_files path else add_file path;
        parse_dir dir;
    with End_of_file -> closedir dir; in address |> opendir |> parse_dir

let add (address : string) : unit = 
  try
    if Sys.is_directory ".git-ml" 
    then 
      begin
        if Sys.is_directory address 
        then add_dir_files address
        else add_file address
      end
  with
  | Unix_error (ENOENT, name, ".git-ml") ->
    print_endline ("fatal: Not a git-ml repository" ^
                   " (or any of the parent directories): .git-ml")
  | Sys_error msg -> print_endline msg

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
      then failwith "tag already exists" 
      else
        let oc = open_out (".git-ml/refs/tags/" ^ str) in
        output_string oc commit_hash;
        close_out oc
    with
    | Unix_error _ -> ()

(** [tree_hash_to_git_tree hash_adr] is the GitTree.t of the Tree_Object at
    hash_adr. 
    Requires: [hash_adr] is a valid hash adress to a Tree_Object *)
let rec tree_hash_to_git_tree name hash_adr =
  let rec helper (lst:string list) acc : GitTree.t list =
    match lst with 
    | [] -> acc
    | h::tail -> match String.split_on_char ' ' h with 
      | [] -> failwith "Error, helper operating on empty string"
      | h::t when h = "" -> helper tail acc 
      | h::t when h = "Tree_Object" -> 
        helper tail ((tree_hash_to_git_tree (List.nth t 1) (List.nth t 0))::acc)
      | h::t when h = "File" -> 
        helper tail (Node ((File (List.nth t 1)), 
                           ((helper ((cat_string (List.nth t 0)) 
                                     |> (String.split_on_char '\n')) []
                            )))::acc)
      | h::t when h = "Blob" -> 
        Node ((Blob 
                 (let s = List.fold_left (fun a b -> a ^ " " ^ b) "" t in  
                  String.sub s 1 (String.length s - 1)))
             ,[])::acc
      | h::t -> failwith 
                  ("helper only operates on Tree_Object, File or Blob," ^
                   "attempting to operate on:" ^ h ^ 
                   "| with rest of string:" ^ 
                   (List.fold_left (fun a b -> a ^ " " ^ b) "" t))
  in
  let spl = (
    cat_string hash_adr |>
    String.split_on_char '\n') in
  let children = helper spl [] in Node (Tree_Object name, children)


let current_head_to_git_tree () =
  let commit_path = input_line (open_in ".git-ml/HEAD") in
  if not (Sys.file_exists (".git-ml/" ^ commit_path))
  then raise (FileNotFound ("No such file: " ^ (".git-ml/" ^ commit_path)))
  else try
      let commit_hash = input_line (open_in (".git-ml/" ^ commit_path)) in
      cat_string commit_hash |> String.split_on_char '\n' |>
      List.hd |> String.split_on_char ' ' |> List.tl |> List.hd |>
      tree_hash_to_git_tree ""
    with e -> raise e

let file_list_from_index () =
  let rec helper acc = function
    | [] -> acc
    | h::t when h = "" -> helper acc t
    | h::t -> helper (((List.nth (String.split_on_char ' ' h) 0),
                       ((cat_string (List.nth (String.split_on_char ' ' h) 1)) 
                        |> Util.remove_blob))::acc) t
  in
  try
    let index_in = open_in ".git-ml/index" in
    let index_contents = read_file index_in in
    String.split_on_char '\n' index_contents |>
    helper []
  with e -> failwith ".git-ml/index, try git-ml add or git-ml init"

let commit_command message branch =
  try 
    let commit_path = input_line (open_in ".git-ml/HEAD") in
    if not (Sys.file_exists (".git-ml/" ^ commit_path))
    then
      commit message branch 
        (file_list_from_index ()) GitTree.empty_tree_object
    else 
      commit message branch 
        (file_list_from_index ()) (current_head_to_git_tree ())
  with e ->
    commit message branch 
      (file_list_from_index ()) GitTree.empty_tree_object

let commit_command_default () = 
  commit_command "no commit message provided" "master" 

let compare_files hash file_name = 
  let len = file_name |> String.length in
  let len' = len - 2 in
  let str = String.sub file_name 2 len' in
  hash_file str = hash

let rec compare_file_blob prefix f l acc = 
  match l with
  | [Node (Blob b, l')] -> if not (compare_files b (prefix^"/"^f)) 
    then (f::acc) else acc
  | _ -> failwith "A file must have one and only one blob child"

let rec status1_help address lst tree : string list = 
  match tree with
  | Leaf -> failwith "there should be no Leaf in the tree"
  | Node (Tree_Object treeob, l) -> begin
      match l with 
      | [] -> lst
      | h::t -> status1_help (address ^ "/" ^treeob) lst h 
    end
  | Node (Blob b, l) -> failwith "cannot reach any blob"
  | Node (File f, l) -> compare_file_blob address f l lst
  | Node (Commit c, l) -> failwith "cannot reach any commit"
  | Node (Ref r, l) -> failwith "cannot reach any ref"

(* difference between workding directory (tree) and current commit *)
let status1 () = status1_help "" [] (current_head_to_git_tree ())

let status () = 
  let lst = status1 () in
  match lst with
  | [] -> ()
  | h::t -> print_endline h ; ()