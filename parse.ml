(* Check if there are multiple tags then it follows that there are groups
   and it will parse groups. *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

type tag = string * spec
type verb = { name : string; usage : string; default : spec; tags : tag list }

exception Parse_err of string

(** [make_usage usge verbs] is a string describing possible verbs 
    and a short description of the purpose of the commands *)
let make_usage usage verbs =
  "Usage: " ^ usage ^ "\n" ^ 
  (List.fold_left (fun acc verb -> ("\t" ^ verb.name ^ "\t" ^ verb.usage)::acc) 
     ["\t" ^ "help" ^ "\t\t" ^ "Display available commands."] verbs
   |> String.concat "\n")

(** [check_arg_tag lst] true if the first string does not begin with '-'
    in a list of strings, false otherwise *)
let check_arg_tag = function 
  | [] -> true
  | hd::tl -> Str.string_before hd 1 <> "-"

let eval args spec =
  match spec with
  | String f when args <> [] -> List.hd args |> f
  | Unit f -> f ()
  | _ -> raise (Parse_err "Missing arguments for given command.")

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

(** [add_help_verb usg_msg verbs] is [verbs] with an added "help" 
    verb *)
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