structure OrError : OR_ERROR =
struct
  type 'a t = ('a, Error.t) Result.t

  val bind = Result.bind
  val op >>= = Result.>>=
  val >>| = Result.>>|
  val return = Result.return

  fun errorString s =
    Result.fail (Error.fromString s)

  fun errorThunk thunk =
    Result.fail (Error.fromThunk thunk)
end
