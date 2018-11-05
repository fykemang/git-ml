(* To DO: Rewrite the args array as a stream *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

type opt = { name:string; usage:string; action:spec; }
type cmd = { name:string; usage:string; default:spec; opts:opt list }

type error =
  | Cmd_not_found
  | Arg_not_found
  | Opt_not_found of string

exception Parse_error of error * string
exception Duplicate_cmd_error

let usage_string usage cmds =
  let fold_helper key (usage, default, opts) acc =
    if (usage <> "")
    then (Format.sprintf "@[@;<4 0>%-15s%s@]" key usage)::acc 
    else acc in
  Hashtbl.fold fold_helper cmds [Format.sprintf "\nUsage: %s\nCommands:" usage]
  |> List.rev |> String.concat "\n"

(* [cmd_usage_string usage opts] describes how to use a specific cmd and lists all
   possible options for the given command. *)
let cmd_usage_string name usage opts =
  let fold_helper key (usage, _) acc =
    if (usage <> "")
    then (Format.sprintf "@[@;<4 0>%-15s%s@]" key usage)::acc
    else acc in
  if Hashtbl.length opts = 0
  then Format.sprintf "\n%s: %s\nOptions: No options available." name usage 
  else Hashtbl.fold fold_helper opts 
      [Format.sprintf "\n%s: %s\nOptions:" name usage]
       |> List.rev |> String.concat "\n"

let print_usage usage cmds = print_endline (usage_string usage cmds)

(** [check_arg_opt lst] true if the first string does not begin with '-'
    in a list of strings, false otherwise *)
let check_arg_opt = function
  | [] -> true
  | hd::tl -> Str.string_before hd 1 <> "-"

(** [eval args spec] evaluates the next argument in [args] based on [spec]
    Raises: [Parse_error err s] if the next argument in [args] is missing and 
    cannot be processed with [spec] *)
let eval (args : string list) = function
  | String f when args <> [] -> String.concat " " args |> f
  | Unit f when args = [] -> f ()
  | _ -> raise (Parse_error (Arg_not_found, "can't evaluate missing or " ^
                                            "extraneous arguments"))

(** [parse_opts args opts] evaluates the next argument in [args] based on
    [opts]
    Raises: [Parse_error err s] if the next argument is not a opt in [opts] *)
let parse_opts (args : string list) (curr_cmd : string) opts : unit =
  match args with
  | [] -> raise (Parse_error ((Opt_not_found curr_cmd), "option not found"))
  | hd::tl when not (check_arg_opt tl) -> 
    raise (Parse_error ((Opt_not_found curr_cmd), "double option not allowed"))
  | hd::tl -> match Hashtbl.find_opt opts hd with
    | None -> raise (Parse_error (
        (Opt_not_found curr_cmd), "invalid option for the given command"))
    | Some (_, spec) -> eval tl spec

(** [parse_cmd args cmds] evaluates the next argument in [args]
    based on [cmds]
    Raises: [Cmd_not_found s] if the next argument in [args] is not 
            a command in [cmds] *)
let parse_cmd (args : string list) cmds : unit  =
  match args with
  | [] -> raise (Parse_error (Cmd_not_found, "command not found"))
  | hd::tl -> match Hashtbl.find_opt cmds hd with
    | None -> raise (Parse_error (Cmd_not_found, "command not found"))
    | Some (_, default, opts) when check_arg_opt tl -> eval tl default
    | Some (_, _, opts) -> parse_opts tl hd opts

(** [add_help_verb usg_msg cmds] is [cmds] initialized with default help 
    commands *)
let init_default_cmds (usg_msg : string) cmds =
  Hashtbl.add cmds "help" (
    "Display available commands.",
    Unit (fun () -> print_usage usg_msg cmds),
    Hashtbl.create 10
  );
  Hashtbl.add cmds "--help" (
    "",
    Unit (fun () -> print_usage usg_msg cmds),
    Hashtbl.create 10
  ); cmds

(** [opts_to_table opts] is a hashtable with mappings from the name of the
    option to a tuple of string [usage] and spec [action]
    Raises: [Duplicate_cmd_error] if there are duplicate options *)
let opts_to_table (opts : opt list) = 
  let tbl = Hashtbl.create(List.length opts) in
  List.fold_left (
    fun acc ({name; usage; action} : opt) ->
      if Hashtbl.mem acc name
      then raise Duplicate_cmd_error
      else Hashtbl.add acc name (usage, action); acc;
  ) tbl opts

(** [cmds_to_table cmds] is a hashtable with mappings from the name of the
    command to a tuple of string [usage], spec [default], and a
    hashtable of options [opts]
    Raises: [Duplicate_cmd_error] if there are duplicate commands *)
let cmds_to_table (cmds : cmd list) =
  let tbl = Hashtbl.create(List.length cmds) in
  List.fold_left (
    fun acc {name; usage; default; opts} ->
      if Hashtbl.mem acc name
      then raise Duplicate_cmd_error
      else Hashtbl.add acc name (usage, default, opts_to_table opts); acc;
  ) tbl cmds

(** [find_cmd_opts cmd cmds] are the options of a command [cmd]
    Raises: [Parse_error] if the command does not exist in [cmds] *)
let find_cmd_opts (cmd : string) cmds =
  match Hashtbl.find_opt cmds cmd with
  | None -> raise (Parse_error (Cmd_not_found, "command not found"))
  | Some (usage, default, opts) -> opts

(** [find_cmd_usage cmd cmds] is the usage message of a command [cmd]
    Raises: [Parse_error] if the command does not exist in [cmds] *)
let find_cmd_usage (cmd : string) cmds =
  match Hashtbl.find_opt cmds cmd with
  | None -> raise (Parse_error (Cmd_not_found, "command not found"))
  | Some (usage, default, opts) -> usage

let parse args usg_msg cmds =
  let init_cmds = cmds |> cmds_to_table |> init_default_cmds usg_msg in
  try
    if List.length args <> 0
    then parse_cmd args init_cmds
    else raise (Parse_error (Cmd_not_found, "no command issued"))
  with
  | Parse_error (err, msg) -> match err with
    | Arg_not_found -> print_endline ("fatal: " ^ msg)
    | Cmd_not_found -> print_endline ("fatal: " ^ msg);
      print_endline (usage_string usg_msg init_cmds);
    | Opt_not_found cmd -> print_endline ("fatal: " ^ msg);
      print_endline (cmd_usage_string cmd (find_cmd_usage cmd init_cmds)
                       (find_cmd_opts cmd init_cmds))