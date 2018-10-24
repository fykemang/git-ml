open Commands
open Parse
open Array
open GitTree

let verb_list = [
  {
    name = "hash-object"; 
    usage = "Hashes a file."; 
    default = Parse.String (hash_object_default);
    tags = [
      ("-w", Parse.String (hash_object))
    ]
  };
  {
    name = "cat-file";
    usage = "Looks for a file based on id.";
    default = Parse.String (cat);
    tags = []
  };
  {
    name = "init";
    usage = "Intializes repository.";
    default = Parse.Unit (Commands.init);
    tags = []
  }
]

let main () = begin
  let args = sub Sys.argv 1 (length Sys.argv - 1) |> to_list in
  parse args "EXECUTABLE [COMMAND] [FLAGS] <INPUT>" verb_list
end

let () = main ()