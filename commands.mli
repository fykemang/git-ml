(** [init ()] initializes a .git-ml directory within the current directory
    with default files and folders. *)
val init : unit -> unit

(** [hash_object s] is the digest of the given string or file after being
    hashed using the MD5 algorithm.  *)
val hash_object : string -> unit

