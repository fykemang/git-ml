open GitTree

let hash_file file = if Sys.file_exists file 
  then file |> Digest.file |> Digest.to_hex 
  else raise (Unix.Unix_error (Unix.ENOENT, "hash_file", file))

let hash_str s = s |> Digest.string |> Digest.to_hex

(** [tree_content_to_file_list pointer] is the file list of type 
    [string*string list] that is a list of filenames and file contents. 
    Mutually recurisve with [cat_file_to_git_object s] 
    Requires: 
      pointer is a valid pointer to a tree*)
let tree_content_to_file_list (pointer:string) =
  failwith "Unimplemented"

(** This may not be at all useful *)
let cat_file_to_git_object (s:string) =
  match String.split_on_char ' ' s with
  |h::t when h = "Blob" -> Blob (List.fold_left (^) "" t )
  |h::t when h = "Tree_Object" -> Tree_Object (List.fold_left (^) "" t ) 
  |_ -> failwith "Unimplemented"

