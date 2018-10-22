(** [init ()] initializes a .git-ml directory within the current directory
    with default files and folders. *)
val init : unit -> unit

(** [hash_object s] is the digest of the given string or file after being
    hashed using the MD5 algorithm. Optionally creates a blob from a file. *)
val hash_object : string -> unit

(** [cat s] is the content or type of an object given [s] its hashed 
    hex representation. *)
val cat : string -> string

(** [log ()] returns a formatted string detailing all commits *)
val log : unit -> string

(** [ls_tree s] is the structure of a tree given [s] its hashed
    hex representation. *)
val ls_tree : string -> string
