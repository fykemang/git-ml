(** Useful helper functions. *)

(** [hash_file file] is the MD5 checksum of the given file [file].
    Raises: [Unix_error] if the file is not found. *)
val hash_file : string -> string

(** [hash_str s] is the MD5 checksum of the given string [s]. *)
val hash_str : string -> string

(** [print_hash s] outputs the hash of a string to stdout *)
val print_hash_str : string -> unit

(** [remove_blob s] is the string with the Blob header removed from s*)
val remove_blob : string -> string

(** [read_file ?s file_chnl] reads the [file_chnl] and outputs the content to 
    [s], it closes [file_chnl] after reaching the end of file. *)
val read_file : ?s:string -> in_channel -> string