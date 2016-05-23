val op >>= = Result.>>=
infix >>=

val result =
  Result.return 5 >>= (fn v =>
  Result.fail (Int.toString 5) >>= (fn g =>
  ( print "Got here\n"
  ; Result.return 3
  )))

val () =
  case result of
    Result.Ok ok => print ("Ok: " ^ (Int.toString ok) ^ "\n")
  | Result.Error err => print ("Error: " ^ err ^ "\n")
