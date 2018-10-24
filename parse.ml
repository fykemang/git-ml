(* Check if there are multiple tags then it follows that there are groups
   and it will parse groups. *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

type tag = string * spec
type verb = { name : string; usage : string; default : spec; tags : tag list }

exception Parse_err of string
exception Verb_not_found

let usage_string usage verbs =
  "Usage: " ^ usage ^ "\n" ^ 
  (
    List.fold_left (
      fun acc verb ->
        if (verb.usage <> "") 
        then ("\t" ^ verb.name ^ "\t" ^ verb.usage)::acc 
        else acc
    )
      ["\t" ^ "help" ^ "\t\t" ^ "Display available commands."] verbs
    |> String.concat "\n"
  )

(** [check_arg_tag lst] true if the first string does not begin with '-'
    in a list of strings, false otherwise *)
let check_arg_tag = function 
  | [] -> true
  | hd::tl -> Str.string_before hd 1 <> "-"

let eval (args : string list) (spec : spec) =
  match spec with
  | String f when args <> [] -> List.hd args |> f
  | Unit f -> f ()
  | _ -> raise (Parse_err "Can't evaluate missing arguments.")

let rec parse_tags (args : string list) (tags : tag list) =
  let tag = List.hd args in
  match List.assoc_opt tag tags with
  | None -> raise (Parse_err "invalid tag for the given command.")
  | Some spec -> eval (List.tl args) spec

let parse_verbs (args : string list) (verbs : verb list)  =
  let fst_arg = List.hd args in
  let tl_arg = List.tl args in
  match List.fold_left (
      fun acc verb ->
        match acc with
        | None -> if verb.name = fst_arg then Some verb else None
        | Some v -> acc
    ) 
      None verbs with
  | None -> raise Verb_not_found
  | Some {name; usage; default; tags} ->
    match tags with
    | [] -> eval tl_arg default
    | tags when check_arg_tag tl_arg -> eval tl_arg default
    | tags -> parse_tags tl_arg tags

(* Will construct text for how to use a verb *)
let make_verb_usage usage tags = failwith "unimplemented"

(** [add_help_verb usg_msg verbs] is [verbs] with an added "help" 
    verb *)
let set_up_verbs (usg_msg : string) (verbs : verb list) : verb list =
  {
    name="help";
    usage="Display available commands.";
    default = Unit (fun () -> print_endline (usage_string usg_msg verbs));
    tags=[]
  }::{
    name="--help";
    usage="Display available commands.";
    default = Unit (fun () -> print_endline (usage_string usg_msg verbs));
    tags=[]
  }::verbs |> List.rev

let parse args usg_msg verbs =
  try
    let init_verbs = set_up_verbs usg_msg verbs in
    if List.length args <> 0
    then parse_verbs args init_verbs
    else raise Verb_not_found
  with
  | Parse_err s -> print_endline ("fatal: " ^ s); 
    print_endline (usage_string usg_msg verbs);
  | Verb_not_found -> 
    print_endline ("fatal: Invalid command.");
    print_endline (usage_string usg_msg verbs);
  | Invalid_argument s -> print_endline "Malformed arguments"