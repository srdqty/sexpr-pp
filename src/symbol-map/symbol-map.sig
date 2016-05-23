signature SYMBOL_MAP =
sig
  structure Symbol : SYMBOL

  type 'a t

  val mkMap : unit -> 'a t
  
  val count : 'a t -> int

  val lookup : 'a t * Symbol.t -> 'a option
  val insert : 'a t * Symbol.t * 'a -> unit
  val remove : 'a t * Symbol.t -> 'a OrError.t

  val app : ('a -> unit) -> 'a t -> unit
  val appi : ((Symbol.t * 'a) -> unit) -> 'a t -> unit

  val fold : (('a * 'b) -> 'b) -> 'b -> 'a t -> 'b
  val foldi : ((Symbol.t * 'a * 'b) -> 'b) -> 'b -> 'a t -> 'b
end
