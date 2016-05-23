functor LinkTableFn
(
  structure Value : VALUE
  structure SymbolMap : SYMBOL_MAP
  sharing type Value.SymbolTable.Symbol.t = SymbolMap.Symbol.t
)
: LINK_TABLE =
struct


structure Value = Value

type t = Value.t SymbolMap.t

val mkLinkTable = SymbolMap.mkMap

val lookup = SymbolMap.lookup

fun insert (map, sym, value) =
  case SymbolMap.lookup (map, sym) of
    NONE => OrError.return (SymbolMap.insert (map, sym, value))
  | SOME _ => 
      OrError.errorThunk (fn () =>
        ( "LinkTable.insert: symbol alread exists in the table: "
        ^ (Value.SymbolTable.Symbol.toString sym)))

val replace = SymbolMap.insert

fun fromList lst = let
  val map = mkLinkTable ()
  fun loop [] = map
    | loop ((sym, value) :: rest) =
        ( replace (map, sym, value)
        ; loop rest)
in
  loop lst
end

val remove = SymbolMap.remove


end
