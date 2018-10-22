
  (** A Tree_Object : git_object is different from a GitTree.t *)
  type git_object =
    | Tree_Object of string
    | Blob of string
    | Commit of string
    | Ref of string

  type t = Leaf | Node of git_object * t list
  exception EmptyTreeException of string  

  let empty = Leaf

  let is_empty = function
    | Leaf -> true
    | _ -> false

  let value = function
    | Leaf -> raise (EmptyTreeException "Empty tree does not have value")
    | Node (o,lst) -> o
