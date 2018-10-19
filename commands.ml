open Unix

let init () = try 
    mkdir ".git-ml" 0o777;
    chdir ".git-ml";
    mkdir "objects" 0o777;
  with 
  | Unix_error (EEXIST, func, file) -> 
    print_endline (file ^ " already exists."); 
