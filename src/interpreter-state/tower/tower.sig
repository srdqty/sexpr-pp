signature TOWER =
sig
  type 'a t
  type phase = int

  val mkTower : unit -> 'a t

  (* Thrown if passed in phase is negative *)
  exception InvalidPhase

  (* Returns NONE if no data was ever stored in the argument phase. *)
  val get : 'a t * phase -> 'a option
  val set : 'a t * phase * 'a -> unit
end
