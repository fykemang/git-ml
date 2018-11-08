(** A [Object] is a value that can be compared and formatted. *)
module type Object = sig
  type t

  (** [compare t1 t2] is [-1] if [t1] is less than [t2],
      [0] if [t1] is equal to [t2], or [1] if [t1] is
      greater than [t2]. *)
  val compare : t -> t -> int

  (** [format] outputs a textual representation of a value of type [t] on
      the given formatter *)
  val format : Format.formatter -> t -> unit
end

(** [Diff_engine] is a module that allows a user to compare
    lists of different  *)
module type Diff_engine = sig
  type obj

  (** Represents  *)
  type diff_obj =
    | Del of obj list
    | Add of obj list
    | Eq of obj list

  (** List of diff_objs representing differences, additions,
      and commonalities 
      Example:
      [Diff.Eq ["Gone"];
      Diff.Del ["withi"];
      Diff.Add ["with"]; 
      Diff.Eq ["the"];
      Diff.Del ["winds"; "part"; "two"]; 
      Diff.Add ["wind"]] *)
  type t = diff_obj list

  (** [diff lst lst'] is a diff_obj list [t] which represents
      all the differences, additions, and commonalities between
      [lst] and [lst'].
      Example:
      [lst] = ["Gone"; "withi"; "the"; "winds"; "part"; "two"]
      [lst'] = ["Gone"; "with"; "the"; "wind"]
      [diff lst lst'] =
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