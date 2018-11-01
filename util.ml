let hash_file file = if Sys.file_exists file 
  then file |> Digest.file |> Digest.to_hex 
  else raise (Unix.Unix_error (Unix.ENOENT, "hash_file", file))

let hash_str s = s |> Digest.string |> Digest.to_hex

let print_hash_str s = print_endline (hash_str s)

let remove_blob s =
  match String.split_on_char ' ' s with
  | h::t when h = "Blob" -> (List.fold_left (fun a b -> a ^ " " ^ b) "" t) 
  | h::t -> failwith "Trying to remove_blob on string containing no Blob header"
  | [] -> s 

let rec read_file ?s:(s = "") file_chnl =
  try
    let cur_line = file_chnl |> input_line in
    read_file file_chnl ~s: (s ^ cur_line ^ "\n")
  with
  | End_of_file -> let _ = file_chnl |> close_in in s

let rec read_dir handle s =
  try
    let cur_file = handle |> Unix.readdir in
    let hash = hash_file cur_file in
    if hash = s then cur_file |> open_in |> read_file
    else read_dir handle s 
  with
  | End_of_file -> let _ = handle |> Unix.closedir in raise Not_found

let rec read_dir_filenames handle s =
  try
    let cur_file = handle |> Unix.readdir in
    if cur_file = "." || cur_file = ".." then read_dir_filenames handle s else
      read_dir_filenames handle (s ^ cur_file ^ "\n")
  with
  | End_of_file -> handle |> Unix.closedir; s