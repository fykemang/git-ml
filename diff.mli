type order = LT | EQ | GT

module type OrderedType = sig
  type t
  val compare : t -> t -> order 
end

module type Diff_engine = sig
  type obj

  (** Represents  *)
  type diff_obj = 
    | Del of obj list
    | Add of obj list
    | Eq of obj list

  type t = diff_obj list

  (** [diff lst lst'] is a list of diff_obj [t] which represents
      the difference between  *)
  val diff : obj list -> obj list -> t
end

module Make (Obj: OrderedType) : Diff_engine with type obj = Obj.t