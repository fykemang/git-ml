let hash_file file = if Sys.file_exists file 
  then file |> Digest.file |> Digest.to_hex 
  else raise (Unix.Unix_error (Unix.ENOENT, "hash_file", file))

let hash_str s = s |> Digest.string |> Digest.to_hex

let print_hash_str s = print_endline (hash_str s)

let remove_object_tag tag s = 
  Str.replace_first (Str.regexp_string (tag ^ " ")) "" s

let rec read_line_custom ?s:(s = "") file_chnl =
  try 
    let cur_byte = input_char file_chnl in
    if cur_byte <> '\n' then  
      (read_line_custom file_chnl ~s: (s ^ (String.make 1 cur_byte)))
    else 
      (s,false,pos_in file_chnl)
  with 
  | End_of_file -> (s,true, pos_in file_chnl)

let rec read_file ?s:(s = "") file_chnl : string =
  let (line, early_term, pos) = read_line_custom file_chnl in
  seek_in file_chnl pos;
  if not early_term then 
    read_file file_chnl ~s: (s ^ line ^ "\n")
  else (close_in file_chnl; (s ^ line))


let rec read_dir_filenames handle s =
  try
    let cur_file = handle |> Unix.readdir in
    if cur_file = "." || cur_file = ".." then read_dir_filenames handle s else
      read_dir_filenames handle (s ^ cur_file ^ "\n")
  with
  | End_of_file -> handle |> Unix.closedir; s

let is_outside_path path =
  if not (Filename.is_relative path)
  then raise (Sys_error ("fatal: '" ^ path ^ "' is outside repository.")) 
  else begin
    if Str.first_chars path 1 = "." && String.length path >= 2 then 
      begin
        if Str.first_chars path 2 = ".."
        then raise (Sys_error ("fatal: '" ^ path ^ "' is outside repository."))
      end
  end