(** Module which parses the command line *)
type spec = 
  | String of (string -> unit)
  | Unit of (unit -> unit)
  | NoArg

type tag = string * spec
type verb = {name: string; 
             usage: string; 
             default: spec; 
             tags: tag list}

exception Parse_err of string

val parse : string -> verb list -> unit


