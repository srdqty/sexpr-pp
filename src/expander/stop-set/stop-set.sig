signature STOP_SET =
sig


structure Symbol : SYMBOL

type t

val empty : t

val fromList : Symbol.t list -> t

val member : t * Symbol.t -> bool

val add : t * Symbol.t -> t

val addList : t * Symbol.t list -> t

val union : t * t -> t

end
