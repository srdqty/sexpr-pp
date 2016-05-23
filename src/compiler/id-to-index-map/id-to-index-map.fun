functor IdToIndexMapFn (SymbolMap : SYMBOL_MAP) : ID_TO_INDEX_MAP =
struct

structure Symbol = SymbolMap.Symbol

structure SM = SymbolMap

type t = int SM.t * int ref

exception NotFound
fun mkMap () = (SM.mkMap (), ref 0)

fun lookupOrInsert ((map, indexRef), symbol) =
  case SM.lookup (map, symbol) of
    NONE => 
      let val index = !indexRef in
        SM.insert (map, symbol, index)
      ; indexRef := index + 1
      ; index
      end
  | SOME index => index

fun mkIndexToIdTable (map, _) = let
  val array = Array.array (SM.count map, Symbol.empty)
in
    SM.appi (fn (sym, i) => Array.update (array, i, sym)) map
  ; Array.vector array
end


end
