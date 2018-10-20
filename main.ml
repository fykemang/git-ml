open Arg
open Commands

let specs = [
  ("-init", Unit (fun () -> init ()), "Intialize a version-control repository.");
  ("-hash", String (fun s -> hash_object s), "Hashes a object")
]

let main = begin
  let usage_msg = "Hello World! This is a test." in
  parse specs print_endline usage_msg
end

let () = main