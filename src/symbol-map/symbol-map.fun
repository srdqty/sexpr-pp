functor SymbolMapFn (Symbol : SYMBOL) : SYMBOL_MAP =
struct


structure Symbol = Symbol

structure SymbolHashKey =
struct
  type hash_key = Symbol.t
  val hashVal = Symbol.hash
  val sameKey = Symbol.=
end

structure HT = HashTableFn (SymbolHashKey)

type 'a t = 'a HT.hash_table

exception NotFound
fun mkMap () = HT.mkTable (8, NotFound)

val count = HT.numItems

fun lookup (ht, sym) = HT.find ht sym

fun insert (ht, sym, value) = HT.insert ht (sym, value)

fun remove (ht, sym) =
  OrError.return (HT.remove ht sym) handle NotFound =>
  OrError.errorThunk (fn () =>
    ( "SymbolMap.remove: symbol does not exist in map: "
    ^ (Symbol.toString sym)
    ))

val app = HT.app
val appi = HT.appi
val fold = HT.fold
val foldi = HT.foldi


end
