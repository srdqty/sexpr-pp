structure Result :> RESULT =
struct
  datatype ('ok, 'err) t =
    Ok of 'ok
  | Error of 'err

  fun bind (Ok ok) f = f ok
    | bind (Error err) _ = Error err

  fun op >>= (r, f) = bind r f

  fun op >>| (Ok ok, f) = Ok (f ok)
    | op >>| (Error err, _) = Error err

  fun return ok = Ok ok

  fun fail err = Error err

  val ok_unit = Ok ()
end
