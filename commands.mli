

type filename = string
type file_content = string
type file_object = filename * file_content

(** [init ()] in itializes a .git-ml directory within the current directory
    with default files and folders. *)
val init : unit -> unit

(** [cat s] is the content or type of an object given [s] its hashed 
    hex representation. *)
val cat : string -> unit

(** [hash_object] implements the function of hash-object*)
val hash_object : string -> unit

(** [hash_object_default] implements the function of hash-object*)
val hash_object_default: string -> unit

(** [log b] prints a formatted string detailing all commits on branch b*)
val log : string -> unit

(** [ls_tree s] is the structure of a tree given [s] its hashed
    hex representation. *)
val ls_tree : string -> string

(** [add address] writes the file address and the the file content hash to the
    index. If [address] is the address of a folder, all files and subfolders 
    inside will be written to the index and added to the objects directory.
    Requires: [address] is a valid file or directory in the same folder 
    where /git-ml is contained *)
val add : string -> unit

(** [rm address] deletes the requested file from the index *)
val rm : string -> unit

(** [tag] pretty prints all the tags that have been added, it prints nothing if
    there are no tags assigned yet*)
val tag : unit -> unit

(** [tag_assign name] tags the current commit with string [name]. It hashes the 
    current commit and stores the result in refs/tags/name. If the tag name 
    already exists, then it raises an exception.
    Requires: name cannot be "." or ".." *)
val tag_assign: string -> unit

(** [status ()] compares files in the working directory with the 
    staging directory and prints files which have been modified *)
val status: unit -> unit

(** [current_head_to_git_Tree ()] gives the GitTree represented by the current 
    head **)
val current_head_to_git_tree: unit -> GitTree.t

(** [commit_command message branch] commits the staging area with 
    message [message] on branch [branch] *)
val commit_command: string -> string -> unit

(** [commit_command message branch] commits the staging area with 
    message ["no commit message"] on branch ["branch"] *)
val commit_command_default: unit -> unit