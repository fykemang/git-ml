(** Parses command line arguments. *)

(** Specifications which map an argument type to a function. *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)


exception Parse_err of string
exception Verb_not_found

(** tag is defined by a string [name] and a specification [spec]. 
    Short tags should begin with a dash.
    Long tags should begin with double dashes. *)
type tag = string * spec

(** [verb] is defined by a string [name],
    string [usage], spec [default], and a list of tags [tags].
    [name] is the identifier for the verb.
    [usage] describes the verb to be taken.
    [default] is the specification for an verb if it is given no tags.
    [tags] modify the verb.
    If no [usage] is specified, the verb will not be displayed in the default
    help and --help commands. *)
type verb = { name: string; usage: string; default: spec; tags: tag list }

(** [parse args usg_msg verbs] performs operations defined by [verbs] 
    based on the given list of arguments [args]. [usg_msg] describes 
    the purpose of the verbs. *)
val parse : string list -> string -> verb list -> unit

(** [make_usage usage verbs] is a string describing possible verbs 
    and a short description of the purpose of each of the verbs *)
val usage_string : string -> verb list -> string


