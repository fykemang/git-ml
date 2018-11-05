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
    usage = "Add file/directory to the staging area.";
    default = Parse.String (add);
    opts = []
  };
  {
    name = "rm";
    usage = "Remove file from the staging area.";
    default = Parse.String (rm);
    opts = []
  };
  {
    name = "tag";
    usage = "Get tags";
    default = Parse.Unit (tag);
    opts = [
      {
        name = "-assign"; 
        usage = "Stores hashed object in directory"; 
        action = Parse.String (tag_assign)
      }
    ]
  };
  {
    name = "commit";
    usage = "Commit the staged changes to the commit tree";
    default = Parse.Unit (commit_command_default);
    opts = [
      {
        name = "-m"; 
        usage = "Commit message"; 
        action = Parse.String (fun b -> commit_command b "master")
      }
    ]
  };
  {
    name = "log";
    usage = "Get the ref log of the commits";
    default = Parse.Unit (fun () -> (log "master"));
    opts = [
      {
        name = "-b"; 
        usage = "Get the reflog with on the branch -b [branch]"; 
        action = Parse.String (log)
      }
    ]
  };
  {
    name = "status";
    usage = "Checking status.";
    default = Parse.Unit (status);
    opts = []
  };
]

let main () = begin
  let args = sub Sys.argv 1 (length Sys.argv - 1) |> to_list in
  parse args "EXECUTABLE [COMMAND] [OPTIONS] <INPUT>" cmd_list
end

let () = main ()