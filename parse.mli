(** Extremely basic command line parser geared toward git style of
    commands *)

(** Specifications which map an argument type to a function. *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)

(** Different types of parsing errors *)
type error = 
  | Cmd_not_found
  | Arg_not_found
  | Opt_not_found of string

(** Raised whenever an error is encountered while parsing. The first 
    component is the error code and the second component is the 
    error message *)
exception Parse_error of error * string

(** Raised whenever there are duplicate commands or options in any set of
    commands that are being used to parse *)
exception Duplicate_cmd_error

(** tag is defined by a string [name] and a specification [spec]. 
    Short tags should begin with a dash.
    Long tags should begin with double dashes. *)
type opt = { name:string; usage:string; action:spec; }

(** [cmd] is defined by a string [name],
    string [usage], spec [default], and a list of tags [tags].
    [name] is the identifier for the cmd.
    [usage] describes the cmd to be taken.
    [default] is the specification for an cmd if it is given no tags.
    [tags] modify the cmd.
    If no [usage] is specified, the cmd will not be displayed in the default
    help and --help commands. *)
type cmd = { name: string; usage: string; default: spec; opts: opt list }

(** [parse args usg_msg cmds] performs operations defined by [cmds] 
    based on the given list of arguments [args]. [usg_msg] describes 
    the purpose of the cmds. 
    Raises: [Duplicate_cmd_error] if given two cmds with the same name
           in the cmd list *)
val parse : string list -> string -> cmd list -> unit

(** [make_usage usage cmds] is a string describing possible cmds 
    and a short description of the purpose of each of the commands in [cmds] *)
val usage_string : string -> 
  (string, string * spec * (string, string * spec) Hashtbl.t) Hashtbl.t -> 
  string

(** [print_usage usage verbs] prints out the output from [usage_string] 
    to stdout *)
val print_usage : string -> 
  (string, string * spec * (string, string * spec) Hashtbl.t) Hashtbl.t -> unit


