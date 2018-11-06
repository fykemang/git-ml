module type OrderedType = sig
  type t
  val compare : t -> t -> int
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

module Make (Obj : OrderedType) = struct
  type obj = Obj.t

  type diff_obj =
    | Del of obj list
    | Add of obj list
    | Eq of obj list

  type t = obj list

  module ObjMap = Map.Make(Obj)

  let map_to_indice lst =
    let lst' = List.mapi (fun i obj -> i, obj) lst in
    List.fold_left
      (fun acc (i, obj) -> 
         ObjMap.update obj
           (fun indices -> match indices with
              | None -> Some []
              | Some lst -> Some (i::lst)) acc) ObjMap.empty lst'

  (** [longest_subsequence ?st_s ?st_s' ?seq s s'] is the indice  *)
  let longest_subsequence ?st_s:(st_s = 0) ?st_s':(st_s' = 0)
      ?seq:(seq = 0) lst lst' = 
      let obj_to_indice = map_to_indice lst in

end 