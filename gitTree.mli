module type GitTreeSig = sig 
  (** [t] is the type of a git tree**)
  type t  

  (** A [git_object] is the type of the elements of a git tree**)
  type git_object

  (** [value t] gives the [git_object] at the root node of GitTree.t*)
  val value: t -> git_object 

  (** [empty] is the empty GitTree*)
  val empty : t

  (** [add child o t] is the [GitTree.t] with the child o added to the root node*)
  val add_child: git_object -> t -> t  

  (** [add child tree tree] is the [GitTree.t] with 
      the tree subtree added as a child to the root node of tree*)
  val add_child_tree: t -> t -> t  

  (** [add_file filename content tree] is the [GitTree.t] with the [filename] with
      content [content] added to the root node of tree **)
  val add_file: string -> string -> t -> t

  (** [get_subdirectory subdirectory tree] is the [GitTree.t] corresponding to 
      the subdirectory subdirectory in [tree] if there is such a subdirectory in 
      tree, otherwise it is a fresh subdirectory

      A subdirectory node is a node of type [Tree_Object name] with 
      [name = subdirectory]*)
  val get_subdirectory_tree: string -> t -> t 
end

module GitTree: GitTreeSig 
