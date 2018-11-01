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

(** [log ()] returns a formatted string detailing all commits *)
val log : unit -> string

(** [ls_tree s] is the structure of a tree given [s] its hashed
    hex representation. *)
val ls_tree : string -> string

(** [file_list_to_tree file_list] is the [GitTree] constructed from 
    [file_list]. *)
val file_list_to_tree: file_object list -> GitTree.t 

(** [add file] writes to the index of the repository data about
    [file] if it exists. *)
val add : string -> unit

(** [tag] pretty prints all the tags that have been added, it prints nothing if
    there are no tags assigned yet*)
val tag : unit -> unit

(** [tag_assign name] tags the current commit with string [name]. It hashes the 
    current commit and stores the result in refs/tags/name. If the tag name 
    already exists, then it raises an exception.
    Requires: name cannot be "." or ".." *)
val tag_assign: string -> unit


val status: unit -> unit

(** [commit message branch lst] commits the file_object list into the 
    .git-ml/objects hashtable and creates a commit in master/[branch] **)
val commit: string -> string -> file_object list -> GitTree.t -> unit

(** [current_head_to_git_Tree ()] gives the GitTree represented by the current 
    head **)
val current_head_to_git_tree: unit -> GitTree.t

(** [commit_command message branch] commits the staging area with 
    message [message] on branch [branch] *)
val commit_command: string -> string -> unit

(** [commit_command message branch] commits the staging area with 
    message ["no commit message"] on branch ["branch"] *)
val commit_command_default: unit -> unit
