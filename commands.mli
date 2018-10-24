type filename = string
type file_content = string
type file_object = filename * file_content

(** [init ()] initializes a .git-ml directory within the current directory
    with default files and folders. *)
val init : unit -> unit

(** [print_hash s] prints out the hash of a given object *)
val print_hash : string -> unit

(** [print_hash_file f] prints out the hash of a given object *)
val print_hash_file : string -> unit

(** [cat s] is the content or type of an object given [s] its hashed 
    hex representation. *)
val cat : string -> string

(** [hash_object] implements the function of hash-object*)
val hash_object : string -> unit

(** [log ()] returns a formatted string detailing all commits *)
val log : unit -> string

(** [ls_tree s] is the structure of a tree given [s] its hashed
    hex representation. *)
val ls_tree : string -> string

(** [commit message branch lst] commits the file_object list into the 
    .git-ml/objects hashtable and creates a commit in master/[branch] **)
val commit: string -> string -> file_object list -> unit