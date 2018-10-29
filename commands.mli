type filename = string
type file_content = string
type file_object = filename * file_content

(** [init ()] initializes a .git-ml directory within the current directory
    with default files and folders. *)
val init : unit -> unit

(** [cat s] is the content or type of an object given [s] its hashed 
    hex representation. *)
val cat : string -> unit

(** [hash_object] implements the function of hash-object. *)
val hash_object : string -> unit

(** [hash_object_default] implements the function of hash-object. *)
val hash_object_default: string -> unit

(** [log ()] returns a formatted string detailing all commits. *)
val log : unit -> string

(** [ls_tree s] is the structure of a tree given [s] its hashed
    hex representation. *)
val ls_tree : string -> string

(** [add file] writes to the index of the repository data about
    [file] if it exists. *)
val add : string -> unit

(** [commit message branch lst] commits the file_object list into the 
    .git-ml/objects hashtable and creates a commit in master/[branch]. **)
val commit: string -> string -> file_object list -> unit