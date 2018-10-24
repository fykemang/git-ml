open Commands
open Parse
open Array
open GitTree

let verb_list = [
  {
    name = "hash-object"; 
    usage = "Hashes a file."; 
    default = Parse.String (print_hash_file);
    tags = [
      ("-w", Parse.String (hash_object))
    ]
  };
  {
    name = "cat-file";
    usage = "Looks for a file based on id.";
    default = Parse.String (print_hash);
    tags = []
  };
  {
    name = "init";
    usage = "Intializes repository.";
    default = Parse.Unit (Commands.init);
    tags = []
  };
  {
    name = "add";
    usage = "Add file to the staging area.";
    default = Parse.String (add);
    tags = []
  }
]

let main () = begin
  let args = sub Sys.argv 1 (length Sys.argv - 1) |> to_list in
  parse args "EXECUTABLE [COMMAND] [FLAGS] <INPUT>" verb_list
end

let () = main ()