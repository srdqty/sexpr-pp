signature ID_TO_INDEX_MAP =
sig
  structure Symbol : SYMBOL
  type t

  val mkMap : unit -> t

  (*
  * Adds symbol is it doesn't already exist in the table and
  * returns the index the symbol maps to.
  * If the symbol exists, the function simply returns the index
  * the symbol already maps to.
  *)
  val lookupOrInsert : t * Symbol.t -> int

  val mkIndexToIdTable : t -> Symbol.t vector
end
