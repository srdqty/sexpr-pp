signature OR_ERROR =
sig
  type 'a t = ('a, Error.t) Result.t

  val bind : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  val >>= : 'a t * ('a -> 'b t) -> 'b t
  val >>| : 'a t * ('a -> 'b) -> 'b t

  val errorString : string -> 'a t
  val errorThunk : (unit -> string) -> 'a t
end
