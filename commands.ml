open GitTree
open Unix
open Util
module StrMap = Map.Make(String)

type filename = string
type file_content = string
type file_object = filename * file_content

exception FileNotFound of string 
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

let ls_tree s = failwith "Unimplemented"

let log branch =
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
      let n = readdir dir in
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

(** [read_idx curr_idx] takes a string list [curr_idx] representing the current
    entries in the index and returns a mapping from each entries filename
    to content hash *)
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
                           attempting to operate on: " ^ h ^ "| with rest of string:"
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
  else let commit_hash = input_line (open_in (".git-ml/" ^ commit_path)) in
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

let commit_command message branch =
  try
    let commit_path = input_line (open_in ".git-ml/HEAD") in
    if not (Sys.file_exists (".git-ml/" ^ commit_path))
    then commit message branch (commit_idx ()) GitTree.empty_tree_object
    else commit message branch (commit_idx ()) (current_head_to_git_tree ())
  with e -> commit message branch (commit_idx ()) GitTree.empty_tree_object

let commit_command_default () = 
  commit_command "no commit message provided" "master" 


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

let diff () =
  let idx = read_idx () in
  let wrking_idx = wrking_tree_idx () in
  ()

(*------------------------------status code ---------------------------------*)

let compare_files blob_obj file_name = 
  let len = file_name |> String.length in
  let len' = len - 2 in
  let str = String.sub file_name 2 len' in
  let content = read_file (str |> open_in) in
  hash_str "Blob " ^ content = hash_str "Blob " ^ blob_obj

let rec compare_file_blob prefix f l acc = 
  match l with
  | [Node (Blob b, l')] -> begin
      if compare_files b (prefix ^ "/" ^ f) 
      then acc else f::acc
    end
  | _ -> failwith "A file must have one and only one blob child"

let rec status2_help address acc tree = 
  (** [status1_help_children acc' l] is the updated [acc'] after processing 
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
  | Node (Tree_Object treeob, l) -> status2_help_children 
                                      (address ^ "/" ^ treeob) acc l
  | Node (File f, l) -> compare_file_blob address f l acc
  | Node (Blob b, l) -> failwith "cannot reach any blob"
  | Node (Commit c, l) -> failwith "cannot reach any commit"
  | Node (Ref r, l) -> failwith "cannot reach any ref"

(* difference between working directory (tree) and current commit: 
   paths that have differences between the working tree and the index file *)
let status2 () = status2_help "" [] (current_head_to_git_tree ())

let rec print_list = function 
  | [] -> ()
  | h::t -> print_endline h ; print_list t

(* untracked files, need to add then commit: 
   paths in the working tree that are not tracked by Git 
   let status3 = failwith "TODO" *)

let get_file's_blob_hash = function
  | [Node (Blob b, l')] -> b
  | _ -> failwith "A file must have one and only one blob child"

let rec find_file (address : string) (filename : string) (tree : GitTree.t) : string = 
  let rec find_help_children
      (filename: string)
      (level_addr : string) 
      (l: GitTree.t list) : string = 
    match l with
    | [] -> ""
    | Leaf::t-> failwith "there should be no leaf in the tree"
    | Node (o, lst) as node :: t -> 
      let potential_hash = find_file level_addr filename node in 
      if potential_hash = "" then find_help_children filename level_addr t 
      else potential_hash
  in
  match tree with
  | Leaf -> failwith "there should be no Leaf in the tree"
  | Node (Tree_Object treeob, l) -> find_help_children 
                                      filename (address ^ "/" ^ treeob) l
  | Node (File f, l) -> 
    if (let len = (address ^ "/" ^ f) |> String.length in
        let len' = len - 2 in
        let str = String.sub (address ^ "/" ^ f) 2 len' in 
        str = filename) then (get_file's_blob_hash l) else ""
  | Node (Blob b, l) -> failwith "cannot reach any blob"
  | Node (Commit c, l) -> failwith "cannot reach any commit"
  | Node (Ref r, l) -> failwith "cannot reach any ref"

let file_changed (filename : string) (hash : string) : bool = 
  let hash_in_head = find_file "" filename (current_head_to_git_tree ()) in 
  hash <> hash_str ("Blob " ^ hash_in_head)

(* added but not committed files: 
   paths that have differences between the index file and the current HEAD commit *)
let status1 () : string list = 
  let idx = read_idx () in 
  let updated_map = StrMap.filter file_changed idx in 
  let bindings = StrMap.bindings updated_map in
  List.split bindings |> fst

let status () = 
  let lst1 = status1 () |> List.sort_uniq (String.compare) in 
  if List.length lst1 > 0 then
    print_endline("The following files are about to be commited:");
  print_list lst1;
  let lst2 = status2 () |> List.sort_uniq (String.compare) in 
  if List.length lst2 > 0 then
    print_endline("The following files have been modified since the last commit:");
  print_list lst2
