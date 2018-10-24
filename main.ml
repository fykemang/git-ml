open Commands
open Parse
open Array

let verb_list = [
  {
    name = "hash-object"; 
    usage = "Hashes a file."; 
    default = Parse.String (print_hash);
    tags = [
      ("-w", Parse.String (print_hash))
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
  let args = sub Sys.argv 1 (length Sys.argv - 1) |> to_list in
  parse args "executable [COMMAND] [FLAGS] <INPUT>" verb_list
end

let () = main ()