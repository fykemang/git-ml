
  (** [t] is the type of a git tree**)
  type t  

  (** A [git_object] is the type of the elements of a git tree**)
  type git_object

  (** [value t] gives the [git_object] at the root node of GitTree.t*)
  val value: t -> git_object 

  (** [empty] is the empty GitTree*)
  val empty : t
