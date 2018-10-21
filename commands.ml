let init () = begin
  try
    let curr_dir = Unix.getcwd () in
    Unix.mkdir ".git-ml" 0o700;
    Unix.chdir ".git-ml";
    Unix.mkdir "objects" 0o700;
    print_endline ("Initialized git-ml repository in " ^ curr_dir);
  with
  | Unix.Unix_error (EEXIST, func, file) ->
    print_endline (file ^ " already exists.");
end

<<<<<<< HEAD
let init () = try 
    mkdir ".git-ml" 0o777;
    chdir ".git-ml";
    mkdir "objects" 0o777;
  with 
  | Unix_error (EEXIST, func, file) -> 
    print_endline (file ^ " already exists.");

=======
let hash_object s =
  if Sys.file_exists s then s |> Digest.file |> Digest.to_hex |> print_endline
  else s |> Digest.string |> Digest.to_hex |> print_endline
>>>>>>> 79d35c41471e71554dec2074dcbc22703a744bef
