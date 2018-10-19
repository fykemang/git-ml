open Arg
open Commands

let specs = [
  ("-init", Unit (fun () -> init ()), "Intialize a version-control repository.")
]

let main = begin
  let msg = "Hello World!" in
  parse specs print_endline msg
end

let () = main