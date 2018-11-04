let hash_file file = if Sys.file_exists file 
  then file |> Digest.file |> Digest.to_hex 
  else raise (Unix.Unix_error (Unix.ENOENT, "hash_file", file))

let hash_str s = s |> Digest.string |> Digest.to_hex

let print_hash_str s = print_endline (hash_str s)

let remove_object_tag tag s =
  match String.split_on_char ' ' s with
  | [] -> s
  | h::t when h = tag -> String.concat " " t
  | h::t -> failwith "Trying to remove_blob on string containing no Blob header"

let rec read_file ?s:(s = "") file_chnl =
  try
    let cur_line = input_line file_chnl in
    read_file file_chnl ~s: (s ^ cur_line ^ "\n")
  with
  | End_of_file -> close_in file_chnl; s

let rec read_dir_filenames handle s =
  try
    let cur_file = handle |> Unix.readdir in
    if cur_file = "." || cur_file = ".." then read_dir_filenames handle s else
      read_dir_filenames handle (s ^ cur_file ^ "\n")
  with
  | End_of_file -> handle |> Unix.closedir; s

let is_outside_path path =
  if Str.first_chars path 2 = ".." || Str.first_chars path 1 = "/" 
  then raise (Sys_error ("fatal: '" ^ path ^ "' is outside repository."))