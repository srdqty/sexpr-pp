signature LINK_TABLE =
sig


structure Value : VALUE

type t

val mkLinkTable : unit -> t

val fromList : (Value.SymbolTable.Symbol.t * Value.t) list -> t

val lookup : t * Value.SymbolTable.Symbol.t -> Value.t option

val insert : t * Value.SymbolTable.Symbol.t * Value.t -> unit OrError.t

val replace : t * Value.SymbolTable.Symbol.t * Value.t -> unit

val remove : t * Value.SymbolTable.Symbol.t -> Value.t OrError.t


end
