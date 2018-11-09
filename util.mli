(** Useful helper functions. *)

(** [hash_file file] is the MD5 checksum of the given file [file].
    Raises: [Unix_error] if the file is not found. *)
val hash_file : string -> string

(** [hash_str s] is the MD5 checksum of the given string [s]. *)
val hash_str : string -> string

(** [print_hash s] outputs the hash of a string to stdout *)
val print_hash_str : string -> unit

(** [remove_object_tag s tag] is the string with an object tag [tag] 
    removed from s *)
val remove_object_tag : string -> string -> string

(** [read_file ?s file_chnl] reads the [file_chnl] and outputs the content to 
    [s], it closes [file_chnl] after reaching the end of file. *)
val read_file : ?s:string  -> in_channel -> string

(** [read_dir handle s] reads the directory [dir] and outputs the filenames to
    [s], it closes [handle] after reaching the end of file. *)
val read_dir_filenames : Unix.dir_handle -> string -> string

(** [is_outside_path path] checks to ensure that the given path is not
    anything starting with '/' or '..'
    Raises: [Sys_error] if the path starts with '/' or '..'  *)
val is_outside_path : string -> unit