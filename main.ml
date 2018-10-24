open Arg
open Commands
open Parse

let verb_list = [
  {
    name = "hash-object"; 
    usage = "Hashes a file."; 
    default = Parse.String (print_hash);
    tags = [
      ("-t", Parse.String (print_hash))
    ]
  };
  {
    name = "cat-file";
    usage = "Looks for a file based on id.";
    default = Parse.String (print_hash);
    tags = []
  }
]

let main () = begin
  let args = Array.sub Sys.argv 1 (Array.length Sys.argv - 1) 
             |> Array.to_list in
  parse args "git-ml [command] [tags...] [args...]" verb_list
end

let () = main ()