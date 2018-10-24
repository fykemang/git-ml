(** Useful helper functions. *)

(** [hash_file file] is the MD5 checksum of the given file [file].
    Raises: [Unix_error] if the file is not found. *)
val hash_file : string -> string

(** [hash_str s] is the MD5 checksum of the given string [s]. *)
val hash_str : string -> string