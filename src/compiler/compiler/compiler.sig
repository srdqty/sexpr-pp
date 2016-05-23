signature COMPILER =
sig


structure Value : VALUE

type syntax_object = Value.t
type ast = Value.t Value.Ast.t
type index_to_global_id_table = Value.SymbolTable.Symbol.t vector

(* 
* Compiles a syntax object to an abstract syntax tree and a table
* that maps indexes (used in the AST) to global identifier symbols.
* The table is used in linking the indexes to global values when
* evaluating the AST.
*)
val compile : syntax_object -> (ast * index_to_global_id_table) OrError.t


end
