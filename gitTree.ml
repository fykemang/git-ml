open Util
open Unix

type git_object = 
  | Tree_Object of string
  | Blob of string
  | File of string
  | Commit of string
  | Ref of string

type t = Leaf | Node of git_object * t list
exception EmptyTreeException of string
exception InvalidContentException of string  

let empty = Leaf

let empty_tree_object = Node (Tree_Object ".", [])

let equal_node_value n1 n2 = 
  match n1,n2 with
  | Leaf, Leaf -> true
  | Node (o1, _), Node(o2, _) when o1 = o2 -> true
  | _ -> false 

let root = function
  | Leaf -> raise (EmptyTreeException "Empty tree does not have value")
  | Node (o, lst) -> o

let add_child obj = function
  | Leaf -> Node (obj, [])
  | Node (o, lst) -> Node (o, (Node (obj, [])::lst))

let rec size = function 
  | Leaf -> raise (EmptyTreeException "Empty tree does not have value")
  | Node (Blob _, _) -> 1
  | Node (_, lst) -> 1 + List.fold_left (fun acc y -> (size y) + acc) 0 lst

let value = function
  | Leaf -> raise (EmptyTreeException "Empty tree does not have value")
  | Node (o,lst) -> o

(** [get_subdirectory_helper subdirectory lst] is the [GitTree] constructed 
    from [subdirectory] and [lst]. *)
let rec get_subdirectory_helper (subdirectory:string) = function
  | [] -> Node (Tree_Object (subdirectory), [])
  | h::t when equal_node_value h (Node (Tree_Object subdirectory, [])) -> h
  | h::t -> get_subdirectory_helper subdirectory t 

let get_subdirectory_tree (subdirectory:string) = function
  | Leaf ->  Node (Tree_Object (subdirectory), [])
  | Node (_, lst) -> get_subdirectory_helper subdirectory lst   

(** [add_subtree_to_list tree] ensures child lists do not have duplicate 
    children. *)
let rec add_subtree_to_list tree = function
  | [] -> tree::[]
  | h::t when equal_node_value h tree -> tree::t
  | h::t -> h::(add_subtree_to_list tree t)

let add_child_tree (subtree : t) = function
  | Leaf -> subtree
  | Node (o, lst) -> Node(o, (add_subtree_to_list subtree lst))

let add_file_to_tree (filename:string) (file_content:string) = function
  | Leaf -> add_child_tree (Node ((File filename), 
                                  (Node ((Blob file_content),[])::[]))) empty 
  | Node (o, lst) -> add_child_tree
                       (Node ((File filename), 
                              (Node ((Blob file_content), [])::[]))) 
                       (Node (o, lst))

let string_of_git_object = function
  | Tree_Object s -> s
  | Blob s -> s
  | File s -> s
  | Commit s -> s
  | Ref s -> s

let rec tree_children_content (lst : t list) : string =
  match lst with
  | [] -> ""
  | Node (o,children)::t -> begin
      match o with 
      | Tree_Object s -> "Tree_Object " ^ 
                         (hash_str (tree_children_content children)) 
                         ^ " " ^ s ^ "\n" 
                         ^ (tree_children_content t)
      | File s -> "File " ^ (hash_str (
          "Blob " ^ (string_of_git_object (root (List.hd children))))) 
                  ^ " " ^ s ^ "\n" ^ (tree_children_content t) 
      | _ -> raise (InvalidContentException 
                      ("Tree_Object can only have children with " ^
                       "value of type Tree_Object or File"))
    end
  | h::t -> raise 
              (InvalidContentException "tree_children should not have leaves")

let hash_of_tree (tree: t) : string = 
  match tree with 
  | Leaf -> raise (InvalidContentException "Cannot take hash of leaf")
  | Node (o, lst) ->
    match o with
    | Tree_Object s -> hash_str (tree_children_content lst)
    | _ -> failwith "Unimplemented in accordance with DRY"

(** [write_hash_contents unhashed_adr file_content] writes file_content to 
    the hash of unhashed_adr in the .git-ml/objects hashtable
    It is designed to be used as a helper function **)
let write_hash_contents (unhashed_adr:string) (file_content:string) =
  let hash_addr = hash_str unhashed_adr in
  let fold_header = String.sub hash_addr 0 2  in
  let fold_footer = String.sub hash_addr 2 (String.length hash_addr - 2) in
  let write_to_objects () =
    let oc = open_out (".git-ml/objects/" ^ fold_header ^ "/" ^ fold_footer) in
    output_string oc file_content;
    close_out oc in
  try
    mkdir (".git-ml/objects/" ^ fold_header) 0o700;
    write_to_objects ();
  with 
  | Unix_error _ -> write_to_objects ()

let rec hash_file_subtree = function
  | Leaf -> ()
  | Node (o, lst) -> match o with 
    | Tree_Object s -> write_hash_contents (tree_children_content lst) 
                         (tree_children_content lst);
      List.hd (List.map hash_file_subtree lst); 
    | File s -> write_hash_contents
                  ("Blob " ^ string_of_git_object (root (List.hd lst)))
                  ("Blob " ^ string_of_git_object (root (List.hd lst)))
    | _ -> raise (InvalidContentException "file_subtree can only have nodes
      with value of type Tree_object or File")

let rec pp_git_tree ?acc:(acc = "") (tree:t) = 
  let rec pp_git_tree_children ?acc:(acc = "") = function
    | [] -> acc
    | Leaf::t -> failwith "There should be no leaves in GitTree"
    | Node (o, lst)::t -> pp_git_tree_children t
                            ~acc:(acc ^ pp_git_tree (Node(o,lst)))
  in 
  match tree with
  | Leaf -> ""
  | Node (o, lst) -> match o with 
    | Tree_Object s -> acc ^ (tree_children_content lst) ^ "\n" ^
                       (pp_git_tree_children lst)
    | File s -> acc ^ "File " ^ s ^ "\n" ^ (pp_git_tree_children lst)
    | Blob s -> acc ^ "Blob " ^ s ^ "\n" ^ (pp_git_tree_children lst)
    | _ -> acc ^ "pretty printing not supported for this node" ^ 
           (pp_git_tree_children lst)

let hash_of_git_object = function
  | Blob s -> hash_str ("Blob " ^ s)
  | Commit s -> hash_str ("Commit " ^ s)
  | Ref s -> hash_str ("Ref " ^ s)
  | _ -> failwith "hash_of_git_object does not apply to that GitObject type"

let git_object_of_tree = function
  | Leaf -> failwith "Can't get git_object of Leaf"
  | Node (o,lst) -> o 

let rec mem_hash (hash : string) (t : t) : bool =
  match t with
  | Leaf -> false
  | Node (obj, lst) -> match obj with
    | Tree_Object s | File s | Commit s | Ref s -> 
      if lst = [] then failwith "ERROR: empty list error in mem_hash"
      else List.fold_left (fun acc t -> mem_hash hash t) false lst
    | Blob s -> let obj_hash = ("Blob " ^ s) |> hash_str in
      if obj_hash = hash then true
      else List.fold_left (fun acc t -> mem_hash hash t) false lst

let rec git_object_in_tree_list (target_o:git_object) (tree_lst:t list) : bool =
  match tree_lst with 
  | [] -> false
  | (Node (o, lst))::t when o = target_o -> true
  | h::t -> git_object_in_tree_list target_o t 

let rec matching_tree_in_tree_lst (target_o:git_object) (tree_lst:t list) =
  match tree_lst with 
  | [] -> failwith "target not found"
  | (Node (o, lst))::t when o = target_o -> (Node (o,lst))
  | h::t -> matching_tree_in_tree_lst target_o t 

