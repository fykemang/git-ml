(** Useful helper functions. *)

(** [hash_file file] is the MD5 checksum of the given file [file].
    Raises: [Unix_error] if the file is not found. *)
val hash_file : string -> string

(** [hash_str s] is the MD5 checksum of the given string [s]. *)
val hash_str : string -> string

(** [print_hash s] outputs the hash of a string to stdout *)
val print_hash_str : string -> unit