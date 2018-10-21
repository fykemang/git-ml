open Arg
open Commands

let hash_specs = [
  ("-w")
]

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt]
    to pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [h] -> acc ^ pp_elt h
      | h1::(h2::t as t') -> loop (n+1) (acc ^ (pp_elt h1) ^ "; ") t'
    in loop 0 "" lst
  in "[" ^ pp_elts lst ^ "]"

let specs = [
  ("-init", Unit (fun () -> init ()), "Intialize a version-control repository.");
  ("-hash-object", Arg.Rest (hash_object), "Hashes a object")
]

let main = begin
  let usage_msg = "Hello World! This is a test." in
  let s = Sys.argv in
  print_endline (pp_list (fun s -> s) (Array.to_list s));
  parse specs print_endline usage_msg;
end

let () = main