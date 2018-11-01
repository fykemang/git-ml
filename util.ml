let hash_file file = if Sys.file_exists file 
  then file |> Digest.file |> Digest.to_hex 
  else raise (Unix.Unix_error (Unix.ENOENT, "hash_file", file))

let hash_str s = s |> Digest.string |> Digest.to_hex

let print_hash_str s = print_endline (hash_str s)

let remove_blob s =
  match String.split_on_char ' ' s with
  | h::t when h = "Blob" -> (let s = List.fold_left (fun a b -> a ^ " " ^ b) 
                                 "" t in  
                             String.sub s 1 (String.length s - 1)) 
  | h::t -> failwith "Trying to remove_blob on string containing no Blob header"
  | [] -> s 




