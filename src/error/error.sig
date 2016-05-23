signature ERROR =
sig
  type t

  val fromString : string -> t
  val fromThunk : (unit -> string) -> t

  val toString : t -> string
end
