open Arg

let specs = [
  ("init", String (fun s -> ()), "Intialize a version-control repository.")
]

let main = begin
  let msg = "Hello World!" in
  parse specs print_endline msg
end

let () = main