module type Object = sig
  type t
  val compare : t -> t -> int
  val format : Format.formatter -> t -> unit
end

module type Diff_engine = sig
  type obj

  (** Represents  *)
  type diff_obj =
    | Del of obj list
    | Add of obj list
    | Eq of obj list

  (**  *)
  type t = diff_obj list

  (** [diff lst lst'] is a diff_obj list [t] which represents
      all the differences, additions, and commonalities between
      [lst] and [lst'].
      Example: 
      [Diff.Eq ["Gone"];
      Diff.Del ["withi"];
      Diff.Add ["with"]; 
      Diff.Eq ["the"];
      Diff.Del ["winds"; "part"; "two"]; 
      Diff.Add ["wind"]] *)
  val diff : obj list -> obj list -> t

  (** [format_diff fmt diff] outputs a textual representation 
      of type [t], a diff_obj list *)
  val format_diff : Format.formatter -> diff_obj list -> unit
end

module Make (Obj: Object) : Diff_engine with type obj = Obj.t