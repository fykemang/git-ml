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

(** [log] prints a formatted string detailing all commits on the current
    branch *)
val log : unit -> unit

(** [add address] writes the file address and the the file content hash to the
    index. If [address] is the address of a folder, all files and subfolders 
    inside will be written to the index and added to the objects directory.
    Requires: [address] is a valid file or directory in the same folder 
    where /git-ml is contained *)
val add : string -> unit

(** [rm address] deletes file [address] from the working directory
    and the staging area
    Requires: The file does not have any uncommitted changes staged. *)
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
    staging directory and repository. It shows files that are untracked, 
    modified, or about to be commited*)
val status: unit -> unit

(** [current_head_to_git_Tree ()] gives the GitTree represented by the current 
    head **)
val current_head_to_git_tree: unit -> GitTree.t

(** [commit_command message] commits the staging area with 
    message [message] on the current branch *)
val commit_command: string -> unit

(** [commit_command message branch] commits the staging area with 
    message ["no commit message"] on branch ["branch"] *)
val commit_command_default: unit -> unit

(** [checkout_path folderpath] restores the folderpath to the last commit *)
val checkout_path: string -> unit

val checkout_branch: string -> unit

(** [diff ()] prints out any differences between files in the repository
    and the working directory *)
val diff : unit -> unit

(** [merge_base branches] gives the first common ancesstor between [branches]
    Requires:
    branches is of the form ["branch_a branch_b"] *)
val merge_base: string -> unit

(** [merge_branch_command branches] merges [branches = "branch_a branch_b"] 
    branch b into branch a
    Requires:
        the current working branch is branch_a
        branch_b is not the master branch
        branches is of the form "branch_a branch_b"*)
val merge_branch_command: string -> unit
