module type Object = sig
  type t
  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module type Diff_engine = sig
  type obj
  type diff_obj =
    | Del of obj list
    | Add of obj list
    | Eq of obj list
  type t = diff_obj list
  val diff : obj list -> obj list -> t
end

module Make (Obj : Object) = struct
  type obj = Obj.t

  type diff_obj =
    | Del of obj list
    | Add of obj list
    | Eq of obj list

  type t = diff_obj list

  module Int = struct 
    type t = int let 
    compare = compare 
    let format fmt x = Format.fprintf fmt "%d" x
  end

  module ObjMap = Map.Make(Obj)
  module IntMap = Map.Make(Int)

  let map_to_indice lst =
    let lst' = List.mapi (fun i obj -> i, obj) lst in
    List.fold_left (fun acc (i, obj) -> 
        ObjMap.update obj
          (fun indices -> match indices with
             | None -> Some (i::[])
             | Some lst -> Some (i::lst)) acc) ObjMap.empty lst'

  let rec subseq_helper' st_lst st_lst'
      seq i indices overlap = 
    match indices with
    | [] -> (st_lst, st_lst', seq, overlap)
    | hd::tl -> 
      let new_subseq = (match IntMap.find_opt (hd - 1) overlap with
          | None -> 0
          | Some n -> n) + 1 in
      let overlap = IntMap.add hd new_subseq overlap in
      if new_subseq > seq then
        let st_lst' = i - new_subseq + 1 in
        let st_lst = hd - new_subseq + 1 in
        subseq_helper' st_lst st_lst' new_subseq i tl overlap
      else
        subseq_helper' st_lst st_lst' new_subseq i tl overlap

  let rec subseq_helper ?st_lst:(st_lst = 0) ?st_lst':(st_lst' = 0)
      ?seq:(seq = 0) ?i:(i = 0) ?overlap:(overlap = IntMap.empty) 
      old_lst_indices lst' = 
    match lst' with
    | [] -> (st_lst, st_lst', seq)
    | hd::tl ->
      let indices = match ObjMap.find_opt hd old_lst_indices with 
        | None -> []
        | Some arr -> arr in
      let (st_lst, st_lst', seq, overlap) =
        subseq_helper' st_lst st_lst' seq i indices overlap in
      subseq_helper old_lst_indices tl 
        ~st_lst:st_lst ~st_lst':st_lst' ~seq:seq ~i:(i + 1) ~overlap:overlap

  (** [longest_subsequence ?st_s ?st_s' ?seq s s'] is the indice  *)
  let longest_subsequence lst lst' =
    let obj_to_indice = map_to_indice lst in
    let (st_lst, st_lst', seq ) = subseq_helper obj_to_indice lst' in
    print_endline (string_of_int seq)

  let diff lst lst' = failwith "Unimplemented"
end