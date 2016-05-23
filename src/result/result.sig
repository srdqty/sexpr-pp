signature RESULT =
sig
  datatype ('ok, 'err) t =
    Ok of 'ok
  | Error of 'err

  val bind : ('ok, 'err) t -> ('ok -> ('ok2, 'err) t) -> ('ok2, 'err) t
  val return : 'ok -> ('ok, 'err) t
  (*
  val map : ('ok, 'err) t -> ('ok -> 'ok2) -> ('ok2, 'err) t
  val join : (('ok, 'err) t, 'err) t -> ('ok, 'err) t
  val ignore : ('ok, 'err) t -> (unit, 'err) t
  val all : ('ok, 'err) t list -> ('ok list, 'err) t
  val all_ignore : (unit, 'err) t list -> (unit, 'err) t
  *)

  val >>= : ('ok, 'err) t * ('ok -> ('ok2, 'err) t) -> ('ok2, 'err) t
  val >>| : ('ok, 'err) t * ('ok -> 'ok2) -> ('ok2, 'err) t

  val fail : 'err -> ('a, 'err) t

  val ok_unit : (unit, 'err) t

  (*
  val is_ok : ('ok, 'err) t -> bool
  val is_error : ('ok, 'err) t -> bool

  val ok : ('ok, 'a) t -> 'ok option
  val error : ('a, 'err) t -> 'err option
  *)
end
