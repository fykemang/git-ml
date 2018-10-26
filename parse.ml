type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

type opt = { name:string; usage:string; action:spec; }
type cmd = { name:string; usage:string; default:spec; opts:opt list }

type error =
  | Cmd_not_found
  | Arg_not_found
  | Opt_not_found

exception Parse_error of error * string
exception Duplicate_cmd_error

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

(* [cmd_usage usage opts] describes how to use a specific cmd and lists all
   possible options for the given command. *)
let cmd_usage usage opts = failwith "unimplemented"

let print_usage usage cmds = print_endline (usage_string usage cmds)

(** [check_arg_opt lst] true if the first string does not begin with '-'
    in a list of strings, false otherwise *)
let check_arg_opt = function
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

(** [parse_opts args opts] evaluates the next argument in [args] based on
    [opts]
    Raises: [Parse_error e s] if the next argument is not a opt in [opts] *)
let parse_opts (args : string list) opts : unit =
  match args with
  | [] -> raise (Parse_error (Opt_not_found, "option not found"))
  | hd::tl -> 
    match Hashtbl.find_opt opts hd with
    | None -> raise (Parse_error 
                       (Opt_not_found, "invalid option for the given command"))
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
    | Some (_, default, opts) when Hashtbl.length opts = 0 
                                || check_arg_opt tl -> eval tl default
    | Some (_, default, opts) -> parse_opts tl opts

(** [add_help_verb usg_msg cmds] is [cmds] initialized with default help 
    commands *)
let init_default_cmds (usg_msg : string) (cmds : cmd list) : cmd list =
  let rec new_verbs () =
    {
      name="help";
      usage="Display available commands.";
      default = Unit (fun () -> print_usage usg_msg (new_verbs ()));
      opts=[]
    }::
    {
      name="--help";
      usage="";
      default = Unit (fun () -> print_usage usg_msg (new_verbs ()));
      opts=[]
    }::cmds in
  new_verbs ()


let opts_to_table (opts : opt list) = 
  let tbl = Hashtbl.create(List.length opts) ~random:true in
  List.fold_left (
    fun acc ({name; usage; action} : opt) ->
      if Hashtbl.mem acc name
      then raise Duplicate_cmd_error
      else Hashtbl.add acc name (usage, action); acc;
  ) tbl opts

let cmds_to_table (cmds : cmd list) =
  let tbl = Hashtbl.create(List.length cmds) ~random:true in
  List.fold_left (
    fun acc {name; usage; default; opts} ->
      if Hashtbl.mem acc name
      then raise Duplicate_cmd_error
      else Hashtbl.add acc name (usage, default, opts_to_table opts); acc;
  ) tbl cmds

let parse args usg_msg cmds =
  let init_cmds = init_default_cmds usg_msg cmds in
  let cmds_tbl = cmds_to_table init_cmds in
  try
    if List.length args <> 0
    then parse_cmd args cmds_tbl
    else raise (Parse_error (Cmd_not_found, "no command issued"))
  with
  | Parse_error (e, s) -> match e with
    | Arg_not_found -> print_endline ("fatal: " ^ s)
    | Cmd_not_found -> print_endline ("fatal: " ^ s);
      print_endline (usage_string usg_msg init_cmds);
    | Opt_not_found -> print_endline ("fatal: " ^ s)