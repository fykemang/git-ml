type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

type tag = string * spec
type cmd = { name : string; usage : string; default : spec; tags : tag list }

type error = 
  | Cmd_not_found
  | Arg_not_found
  | Tag_not_found

exception Parse_error of error * string

let usage_string usage cmds =
  Format.sprintf "\nUsage: %s\nCommands:\n" usage ^ (
    List.fold_left (
      fun acc cmd ->
        if (cmd.usage <> "") 
        then (Format.sprintf "@[@;<4 0>%-15s%s@]" cmd.name cmd.usage)::acc 
        else acc
    ) [] cmds
    |> String.concat "\n"
  )

let print_usage usage cmds = 
  print_endline (usage_string usage cmds)

(** [check_arg_tag lst] true if the first string does not begin with '-'
    in a list of strings, false otherwise *)
let check_arg_tag = function
  | [] -> true
  | hd::tl -> Str.string_before hd 1 <> "-"

(** [eval args spec] evaluates the next argument in [args] based on [spec]
    Raises: [Parse_error e s] if the next argument in [args] is missing and 
    cannot be processed with [spec] *)
let eval (args : string list) = function
  | String f when args <> [] -> List.hd args |> f
  | Unit f -> f ()
  | _ -> raise (Parse_error 
                  (Arg_not_found, "can't evaluate missing arguments"))

(** [parse_tags args tags] evaluates the next argument in [args] based on
    [tags]
    Raises: [Parse_error e s] if the next argument is not a tag in [tags] *)
let parse_tags (args : string list) (tags : tag list) : unit =
  let tag = List.hd args in
  match List.assoc_opt tag tags with
  | None -> raise (Parse_error 
                     (Tag_not_found, "invalid tag for the given command"))
  | Some spec -> eval (List.tl args) spec

(** [parse_verbs args cmds] evaluates the next argument in [args]
    based on [cmds]
    Raises: [Cmd_not_found s] if the next argument in [args] is not 
            a command in [cmds] *)
let parse_verbs (args : string list) (cmds : cmd list) : unit  =
  let fst_arg = List.hd args in
  let tl_arg = List.tl args in
  match List.fold_left (
      fun acc cmd -> match acc with
        | None -> if cmd.name = fst_arg then Some cmd else None
        | Some v -> acc
    ) None cmds with
  | None -> raise (Parse_error (Cmd_not_found, "command not found"))
  | Some {name; usage; default; tags} ->
    match tags with
    | [] -> eval tl_arg default
    | tags when check_arg_tag tl_arg -> eval tl_arg default
    | tags -> parse_tags tl_arg tags

(* [make_verb_usage usage tags] describes how to use a specific cmd *)
let make_verb_usage usage tags = failwith "unimplemented"

(** [add_help_verb usg_msg cmds] is [cmds] initialized with default help 
    commands *)
let set_up_verbs (usg_msg : string) (cmds : cmd list) : cmd list =
  let rec new_verbs () =
    {
      name="help";
      usage="Display available commands.";
      default = Unit (fun () -> print_usage usg_msg (new_verbs ()));
      tags=[]
    }::
    {
      name="--help";
      usage="";
      default = Unit (fun () -> print_usage usg_msg (new_verbs ()));
      tags=[]
    }::cmds in
  new_verbs ()

let parse args usg_msg cmds =
  let init_verbs = set_up_verbs usg_msg cmds in
  try
    if List.length args <> 0
    then parse_verbs args init_verbs
    else raise (Parse_error (Cmd_not_found, "no command issued"))
  with
  | Parse_error (e, s) -> match e with
    | Arg_not_found -> print_endline ("fatal: " ^ s)
    | Cmd_not_found -> print_endline ("fatal: " ^ s);
      print_endline (usage_string usg_msg init_verbs);
    | Tag_not_found -> print_endline ("fatal: " ^ s)