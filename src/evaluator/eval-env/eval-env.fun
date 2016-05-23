structure EvalEnv :> EVAL_ENV =
struct


type 'a t = ('a vector) * ('a array list)
type 'a frame = 'a array


fun mkEmpty () = (Vector.fromList [], [])


fun mkFromGlobalTable tbl = (tbl, [])


fun extend ((g, env), frame) = (g, frame :: env)


fun globalRef ((g, _), i) = 
  OrError.return (Vector.sub (g, i)) handle Subscript =>
  OrError.errorThunk
    (fn () => "EvalEnv: invalid global index: " ^ (Int.toString i))


fun frameRef (frame, i) =
  OrError.return (Array.sub (frame, i)) handle Subscript =>
  OrError.errorThunk
    (fn () => "EvalEnv: invalid frame index: " ^ (Int.toString i))


fun shallowRef ((_, []), _) =
      OrError.errorString "EvalEnv.shallowRef: empty environment"
  | shallowRef ((_, frame :: _), i) = frameRef (frame, i)


fun deepRef ((_, env), j, i) = let
  fun deepRefLoop (frame :: _, 0, i) = frameRef (frame, i)
    | deepRefLoop (frame :: rest, j, i) =
        if j < 0 then (
          OrError.errorThunk (fn () => "EvalEnv.deepRef: invalid index")
        ) else (
          deepRefLoop (rest, j - 1, i)
        )
    | deepRefLoop ([], _, _) =
        OrError.errorString "EvalEnv.deepRef: empty environment"
in
  deepRefLoop (env, j, i)
end


fun print ((g, env), outstream, toString) = let
  fun output s = TextIO.output (outstream, s)

  fun printGlobals gbl =
    ( output "(GlobalTable "
    ; output "(Size "
    ; output (Int.toString (Vector.length gbl))
    ; output ") ("
    ; Vector.app (fn x => (output (toString x); output " ")) gbl
    ; output ") "
    )

  fun printFrame frame =
    ( output "(Size "
    ; output (Int.toString (Array.length frame))
    ; output ") ("
    ; Array.app (fn x => (output (toString x); output " ")) frame
    )

  fun printEnv (i, []) = ()
    | printEnv (i, (frame :: rest)) =
        ( output "(Frame (Index "
        ; output (Int.toString i)
        ; output ") "
        ; printFrame frame
        ; output ") "
        ; printEnv (i + 1, rest)
        )
in
  ( output "(EvalEnv "
  ; printGlobals g
  ; printEnv (0, env)
  ; output ")"
  )
end


end
