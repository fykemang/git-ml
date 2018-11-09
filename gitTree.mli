(** A [git_object] is the type of the elements of a git tree
    A Tree_Object : [git_object] is different from a [GitTree.t]. *)
type git_object =
  | Tree_Object of string
  | Blob of string
  | File of string
  | Commit of string
  | Ref of string

(** [t] is the type of a git tree *)
type t = Leaf | Node of git_object * t list 

(** [value t] gives the [git_object] at the root node of [GitTree.t]. *)
val root: t -> git_object

(** [empty] is the empty [GitTree]. *)
val empty : t

(** [empty_tree_object] is a tree with an empty [TreeObject "."] *)
val empty_tree_object: t

(** [size tree] is the number of nodes in [tree] *)
val size: t -> int

(** [equal_node_value n1 n2] is [true] if [n1] is equal to [n2]. *)
val equal_node_value: t -> t -> bool

(** [add child o t] is the [GitTree.t] with the child [o] added to the 
    root node. *)
val add_child: git_object -> t -> t  

(** [add child tree tree] is the [GitTree.t] with 
    the tree subtree added as a child to the root node of tree. *)
val add_child_tree: t -> t -> t  

(** [add_file filename content tree] is the [GitTree.t] with the [filename] with
    content [content] added to the root node of tree. **)
val add_file_to_tree: string -> string -> t -> t

(** [get_subdirectory subdirectory tree] is the [GitTree.t] corresponding to 
    the subdirectory subdirectory in [tree] if there is such a subdirectory in 
    tree, otherwise it is a fresh subdirectory.

    A subdirectory node is a node of type [Tree_Object name] with 
    [name = subdirectory]. *)
val get_subdirectory_tree: string -> t -> t 

(** [tree_children_content lst] is the string representation of the 
    content of the directory represented by the [lst].
    Requires:
        [lst] is a valid child list of a GitTree Node with value 
        type [Tree_object]. *)
val tree_children_content: t list -> string

(** [write_hash_contents unhashed_adr file_content] writes file_content to 
    the hash of unhashed_adr in the .git-ml/objects hashtable
    It is designed to be used as a helper function. **)
val write_hash_contents: string -> string -> unit

(** [hash_file_subtree tree] hashes and stores the valid contents of the tree
    into the git-ml file system in the objects directory
    Requires:
    [tree] has node with value of type [Tree_Object], [File], or [Blob] *)
val hash_file_subtree: t -> unit

(** [hash_of_tree tree] is the valid md5 hash of a [GitTree] with value of type 
    [Tree_Object] 
    Requires:
        [value tree] is of type [Tree_Object]. *)
val hash_of_tree: t -> string

(** [string_of_git_object] is the string associated with a certain git object 
    [string_of_git_object (File "test.txt")] would evaluate to ["test.txt"]. *)
val string_of_git_object: git_object -> string

(** [pp_git_tree ?acc tree] is the pretty printed string of tree t *)
val pp_git_tree: ?acc:string -> t -> string

(** [hash_of_git_object obj] is the hash of a given [git_object] obj *)
val hash_of_git_object: git_object -> string

(** [mem_file hash t] is true if the blob object with hash [hash] is inside
    GitTree [t], false otherwise *)
val mem_hash: string -> t -> bool

(** [git_object_of_tree tree] is the [git_object] at the root of [tree] *)
val git_object_of_tree: t -> git_object

(** [git_object_of_tree_list target lst] is true if a Node with [git_object]
    equal to target is in the lst *)
val git_object_in_tree_list: git_object -> t list -> bool

(** [matching_tree_in_tree_lst target lst] is the tree in lst with GitObject 
    target *)
val matching_tree_in_tree_lst: git_object -> t list -> t

(** [value t] gives the [git_object] at the root node of [GitTree.t]. *)
val value: t -> git_object 