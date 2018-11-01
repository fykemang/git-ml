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

let rec union ?acc:(acc = []) lst lst' =
  match lst, lst' with
  | [], [] -> acc
  | (file, hash)::tl, [] -> if not (List.mem_assoc file acc)
    then union ~acc:((file, hash)::acc) tl []
    else union ~acc:acc tl []
  | [], (file, hash)::tl -> union ~acc:((file, hash)::acc) [] tl
  | l, (file, hash)::tl -> union ~acc:((file, hash)::acc) l tl
