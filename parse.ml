
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)
  | NoArg

type tag = string * spec
type verb = { name: string; usage: string; 
              default: spec; tags: tag list }

exception Parse_err of string

let get_arg n = Sys.argv.(n + 1)

let check_arg_tag n = Str.string_before (get_arg n) 1 <> "-"

let argc () = Array.length Sys.argv - 1

let parse_arg n = function
  | String f -> n |> get_arg |> f
  | Unit f -> f ()
  | NoArg -> raise (Parse_err "no arguments given for the command.")

let rec parse_tags tags argi = 
  let tag = get_arg argi in
  match List.assoc_opt tag tags with
  | None -> raise (Parse_err "invalid tag for the given command.")
  | Some spec -> parse_arg (argi + 1) spec

let parse_verbs (verbs : verb list) =
  let argi = 0 in
  let fst_arg = get_arg argi in
  match List.fold_left 
          (fun acc verb -> 
             match acc with
             | None -> if verb.name = fst_arg then Some verb else None
             | Some v -> acc) None verbs  with
  | None -> raise (Parse_err "specified command could not be found.")
  | Some {name; usage; default; tags} ->
    match tags with
    | [] -> parse_arg (argi + 1) default
    | tags when check_arg_tag (argi + 1) -> parse_arg (argi + 1) default
    | tags -> parse_tags tags (argi + 1)

(* Will construct text for how to use a verb *)
let make_verb_usage usage tags = failwith "Not implemented"

(* Will construct test for how to use base commands *)
let make_usage usage verbs =
  " Usage: " ^ usage ^ "\n" ^ 
  (List.fold_left (fun acc verb -> ("\t" ^ verb.name ^ "\t" ^ verb.usage)::acc) 
     [] verbs |> List.rev |> String.concat "\n")

let add_help_verb usg_msg verbs = 
  {
    name="help";
    usage="Display available commands.";
    default=Unit (fun () -> print_endline (make_usage usg_msg verbs));
    tags=[]
  }::verbs |> List.rev

let parse usg_msg verbs =
  try
    verbs |> add_help_verb usg_msg |> parse_verbs;
  with
  | Parse_err s -> print_endline ("fatal: " ^ s);
    print_endline (make_usage usg_msg verbs);
  | Invalid_argument s -> print_endline "Malformed arguments"