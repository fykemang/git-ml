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
  val format_diff : Format.formatter -> diff_obj list -> unit
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
    let format fmt x = Format.fprintf fmt "%n" x
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

  let rec subseq_helper' st_lst st_lst' seq i indices overlap = 
    match indices with
    | [] -> (st_lst, st_lst', seq, overlap)
    | hd::tl ->
      let old_subseq = match IntMap.find_opt (hd - 1) overlap with
        | None -> 0
        | Some n -> n in
      let new_subseq = old_subseq + 1 in
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

  (** [longest_subsequence lst lst'] is the tuple 
      [(st_lst, st_lst', seq)] where st_lst and st_lst' are the indices 
      where the longest_subsequence common to both lst and lst' begin 
      respectively and [seq] is the length of the longest common
      subsequence. *)
  let longest_subsequence lst lst' =
    let obj_to_indice = map_to_indice lst in
    subseq_helper obj_to_indice lst'

  let rec fold_n f ?acc:(acc=[]) n = function
    | [] -> acc, []
    | hd::tl as lst -> if n = 0 then acc, lst 
      else fold_n f (n - 1) tl ~acc:(f acc hd)

  (** [slice i k lst] is a sublist of [lst] from index [i] to [k] *)
  let rec slice i k lst = 
    let _, lst = fold_n (fun acc h -> acc) i lst in
    let acc, _ = fold_n (fun acc h -> h::acc) (k - i + 1) lst in
    List.rev acc

  (** [tl_append ?acc lst lst'] is a tail-recursive form of [lst @ lst'] *)
  let rec tl_append ?acc:(acc=[]) lst lst' =
    match lst, lst' with
    | [], [] -> acc |> List.rev
    | [], hd::tl -> tl_append [] tl ~acc:(hd::acc)
    | hd::tl, lst' -> tl_append tl lst' ~acc:(hd::acc)

  (** [drop n lst] is [lst] with the first [n] elements removed *)
  let rec drop n = function
    | [] -> []
    | hd::tl as lst -> if n = 0 then lst else drop (n - 1) tl

  let rec diff lst lst' =
    match lst, lst' with
    | [], [] -> []
    | _, _ ->
      let (st_lst, st_lst', seq) = longest_subsequence lst lst' in
      if seq = 0 then 
        match lst, lst' with
        | [], lst' -> [Add lst']
        | lst, [] -> [Del lst]
        | lst, lst' -> [Del lst; Add lst']
      else
        diff (drop (st_lst + seq) lst) (drop (st_lst' + seq) lst') |>
        tl_append [Eq (slice st_lst' (st_lst' + seq - 1) lst')] |>
        tl_append (diff (slice 0 (st_lst - 1) lst) (slice 0 (st_lst' - 1) lst'))


  let format_diff fmt t = 
    Format.fprintf fmt "[";
    List.iter (fun diff_obj -> 
        match diff_obj with
        | Del lst -> 
          Format.fprintf fmt "Del"; 
          Format.fprintf fmt "[";
          List.iter (fun x -> Format.fprintf fmt "%a, " Obj.format x) lst;
          Format.fprintf fmt "]";
        | Eq lst -> 
          Format.fprintf fmt "Eq";
          Format.fprintf fmt "[";
          List.iter (fun x -> Format.fprintf fmt "%a, " Obj.format x) lst;
          Format.fprintf fmt "]";
        | Add lst -> 
          Format.fprintf fmt "Add";
          Format.fprintf fmt "[";
          List.iter (fun x -> Format.fprintf fmt "%a, " Obj.format x) lst;
          Format.fprintf fmt "]";) t;
    Format.fprintf fmt "]";
end