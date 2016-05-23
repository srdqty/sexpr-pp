structure SymbolTable = SymbolTableFn(struct end)

structure SymbolMap = SymbolMapFn(SymbolTable.Symbol)
structure IdToIndexMap = IdToIndexMapFn(SymbolMap)

structure I = IdToIndexMap
structure S = SymbolTable.Symbol

val map = I.mkMap ()

fun output s = print
  ( "Added id "
  ^ (S.toString s)
  ^ ": "
  ^ (Int.toString (I.lookupOrInsert (map, s)))
  ^ "\n"
  )

val () = output (S.fromString "symbol0")
val () = output (S.fromString "symbol1")
val () = output (S.fromString "symbol2")
val () = output (S.fromString "symbol3")
val () = output (S.fromString "symbol4")
val () = output (S.fromString "symbol5")
val () = output (S.fromString "symbol6")
val () = output (S.fromString "symbol7")
val () = output (S.fromString "symbol8")
val () = output (S.fromString "symbol9")

val () = print "\n"

val () = output (S.fromString "symbol4")
val () = output (S.fromString "symbol3")
val () = output (S.fromString "symbol6")
val () = output (S.fromString "symbol8")
val () = output (S.fromString "symbol9")
val () = output (S.fromString "symbol5")
val () = output (S.fromString "symbol0")
val () = output (S.fromString "symbol7")
val () = output (S.fromString "symbol2")

val table = I.mkIndexToIdTable map

val () = print "\n***** IndexToIdTable *****\n"
val () =
  Vector.appi 
    (fn (i, sym) => print
      ( "table["
      ^ (Int.toString i)
      ^ "] = "
      ^ (S.toString sym)
      ^ "\n"))
    table
