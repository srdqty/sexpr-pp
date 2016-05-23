signature SYMBOL_TABLE =
sig


(* 
* The structure defining operations available on symbols.
* The operations are not thread safe. Each application
* of the SymbolTableFn functor creates a symbol table
* value that is shared for all symbols created using
* the resulting Symbol operations.
*)

structure Symbol : SYMBOL

type t
val table : t

(* Apply a function to all of the symbols in the table. *)
val app : (Symbol.t -> unit) -> unit


end
