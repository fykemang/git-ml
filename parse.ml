
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)
  | NoArg

type tag = string * spec
type verb = { name: string; usage: string; 
              default: spec; tags: tag list }

exception Parse_err of string

let check_arg_tag = function 
  | [] -> true
  | hd::tl -> Str.string_before hd 1 <> "-"

let eval args spec =
  match spec with
  | String f -> List.hd args |> f
  | Unit f -> f ()
  | NoArg -> raise (Parse_err "no arguments given for the command.")

let rec parse_tags args tags  = 
  let tag = List.hd args in
  match List.assoc_opt tag tags with
  | None -> raise (Parse_err "invalid tag for the given command.")
  | Some spec -> eval (List.tl args) spec

let parse_verbs (args : string list) (verbs : verb list)  =
  let fst_arg = List.hd args in
  match List.fold_left 
          (fun acc verb -> 
             match acc with
             | None -> if verb.name = fst_arg then Some verb else None
             | Some v -> acc) None verbs  with
  | None -> raise (Parse_err "specified command could not be found.")
  | Some {name; usage; default; tags} ->
    match tags with
    | [] -> eval (List.tl args) default
    | tags when check_arg_tag (List.tl args) -> eval (List.tl args) default
    | tags -> parse_tags (List.tl args) tags

(* Will construct text for how to use a verb *)
let make_verb_usage usage tags = failwith "Not implemented"

(* Will construct test for how to use base commands *)
let make_usage usage verbs =
  " Usage: " ^ usage ^ "\n" ^ 
  (List.fold_left (fun acc verb -> ("\t" ^ verb.name ^ "\t" ^ verb.usage)::acc) 
     ["\t" ^ "help" ^ "\t\t" ^ "Display available commands."] 
     verbs |> String.concat "\n")

let add_help_verb usg_msg verbs = 
  {
    name="help";
    usage="Display available commands.";
    default=Unit (fun () -> print_endline (make_usage usg_msg verbs));
    tags=[]
  }::verbs |> List.rev

let parse args usg_msg verbs =
  try
    verbs |> add_help_verb usg_msg |> parse_verbs args;
  with
  | Parse_err s -> print_endline ("fatal: " ^ s); 
    print_endline (make_usage usg_msg verbs);
  | Invalid_argument s -> print_endline "Malformed arguments"