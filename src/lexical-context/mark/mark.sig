signature MARK =
sig
  type t

  val null : t
  val freshMark : unit -> t
  val = : t * t -> bool
  val < : t * t -> bool

  val toInt : t -> int
  val toString : t -> string
end
