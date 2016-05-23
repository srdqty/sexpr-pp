functor WriterFn(Datum : WRITER_DATUM) 
  : WRITER where type datum = Datum.t =
struct


type datum = Datum.t

fun makeWriter (name, syntaxPrefix, syntaxSuffix, writeStx, writeAtom) = let
  fun write ws e =
    if Datum.isSyntax e then (
        ws syntaxPrefix
      ; writeStx(e, {writeString = ws,
                     writeDatum = (write ws)})
      ;  ws syntaxSuffix
    ) else if Datum.isNil e then (
      ws "()"
    ) else if Datum.isAtom e then (
      writeAtom (e, ws)
    ) else if Datum.isPair e then (
      writePair (ws, e, true)
    ) else (
      raise (Fail (name ^ ": expected a datum"))
    )
  and writePair(ws, e, first) =
    ( if first then ws "(" else ()
    ; if Datum.isPair e then (
        if Datum.isNil (Datum.cdr e) then (
            write ws (Datum.car e)
          ; ws ")"
        ) else if Datum.isPair (Datum.cdr e) then (
            write ws (Datum.car e)
          ; ws " "
          ; writePair (ws, Datum.cdr e, false)
        ) else (
            write ws (Datum.car e)
          ; ws " . "
          ; write ws (Datum.cdr e)
          ; ws ")"
        )
      ) else (
        raise (Fail (name ^ ": expected a pair"))
      )
    )
in
  write
end

val write =
  makeWriter ("write", "<syntax: ", ">", Datum.writeSyntax, Datum.writeAtom)

val display = 
  makeWriter ("display", "<syntax: ", ">", Datum.writeSyntax, Datum.displayAtom)

val writeRenamedSyntax = 
  makeWriter("write-syntax-with-renames", "", "", 
             Datum.writeRenamedSyntax, Datum.writeAtom)

fun writeSyntax (stx, {writeString, writeDatum}) =
  ( writeString "<syntax: "
  ; writeDatum (Datum.unWrapStx stx)
  ; writeString ">"
  )
val displayVerbose = 
  makeWriter ("display", "", "", writeSyntax, Datum.displayAtom)


end
