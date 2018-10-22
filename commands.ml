open GitTree
open Unix

type filename = string
type file_content = string
type git_index = filename * file_content list

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

let hash_object s =
  if Sys.file_exists s then s |> Digest.file |> Digest.to_hex |> print_endline
  else s |> Digest.string |> Digest.to_hex |> print_endline

(** [hash_string s] is the md5 hash of string s*)
let hash_string s =
  s |> Digest.string |> Digest.to_hex

(** [index_to_tree index] is the [GitTree.t] of  the [git_index] [index]*)
let index_to_tree (index : git_index) =
  failwith "Unimplemented"
