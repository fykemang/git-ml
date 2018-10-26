open Commands
open Parse
open Array
open GitTree

let cmd_list = [
  {
    name = "hash-object"; 
    usage = "Hashes a file."; 
    default = Parse.String (hash_object_default);
    opts = [
      { 
        name = "-w"; 
        usage = "Stores hashed object in directory"; 
        action = Parse.String (hash_object)
      }
    ]
  };
  {
    name = "cat-file";
    usage = "Looks for a file based on id.";
    default = Parse.String (cat);
    opts = []
  };
  {
    name = "init";
    usage = "Intializes repository.";
    default = Parse.Unit (Commands.init);
    opts = []
  };
  {
    name = "add";
    usage = "Add file to the staging area.";
    default = Parse.String (add);
    opts = []
  }
]

let main () = begin
  let args = sub Sys.argv 1 (length Sys.argv - 1) |> to_list in
  parse args "EXECUTABLE [COMMAND] [OPTIONS] <INPUT>" cmd_list
end

let () = main ()