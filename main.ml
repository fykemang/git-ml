open Arg
open Commands

let specs = ref [
    ("-init", Unit (fun () -> init ()), 
     ": Initialize a git-ml repository in the current directory.");
    ("-hash-object", Arg.Rest (hash_object), 
     ": Hashes a object and returns the hash.")
  ]

let main = begin
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ "[-init] [-hash-object string]" in
  parse_dynamic
    specs 
    (fun x -> raise (Arg.Bad ("Bad Argument " ^ x ^ "."))) 
    usage_msg;
end

let () = main