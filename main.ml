open Arg
open Commands

let hash_spec_list = [
  ("-w", String (hash_object), "");
  ("-d", String (hash_object), "")
]

let rec spec_list = ref [
    ("-init", Unit (fun () -> init ()), 
     ": Initialize a git-ml repository in the current directory.");
    ("-hash-object",  Unit (
        fun () -> spec_list := (!spec_list @ hash_spec_list)
      ), 
     ": Hashes a object and returns the hash.");
  ]

let main = begin
  let usage_msg = "These are git.ml commands" in
  parse_dynamic
    spec_list 
    (fun x -> raise (Arg.Bad ("Bad Argument " ^ x ^ "."))) 
    usage_msg;
end

let () = main