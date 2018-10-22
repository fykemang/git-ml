open Arg
open Commands

let args = ref ""

let spec_list = [
    ("-init", Unit (fun () -> init ()), 
     ": Initialize a git-ml repository in the current directory.");
    ("-hash-object", Arg.Set_string (args), 
     ": Hashes a object and returns the hash.")
  ]

let main = begin
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ "[-init] [-hash-object string]" in
  parse
    spec_list 
    (fun x -> raise (Arg.Bad ("Bad Argument " ^ x ^ "."))) 
    usage_msg;
end

let () = print_endline !args 

let () = main