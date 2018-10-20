let init () = begin
  try 
    Unix.mkdir ".git-ml" 0o700;
    Unix.chdir ".git-ml";
    Unix.mkdir "objects" 0o700;
  with 
  | Unix.Unix_error (EEXIST, func, file) -> 
    print_endline (file ^ " already exists."); 
end

let hash_object s =
  s |> Digest.string |> Digest.to_hex |> print_endline
