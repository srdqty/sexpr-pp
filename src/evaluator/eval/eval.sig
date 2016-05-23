signature EVAL =
sig


structure Value : VALUE
structure LinkTable : LINK_TABLE
sharing type Value.t = LinkTable.Value.t
sharing type Value.SymbolTable.Symbol.t 
           = LinkTable.Value.SymbolTable.Symbol.t

type ast = Value.t Value.Ast.t
type environment = Value.t Value.EvalEnv.t
type index_to_global_id_table = Value.SymbolTable.Symbol.t vector
type global_id_to_value_table = LinkTable.t
type phase = int
type mark = Value.LexicalContext.Mark.t
type expand_env = Value.t Value.ExpandEnv.t

val eval : phase * expand_env * mark * environment * ast -> Value.t OrError.t

val link : index_to_global_id_table * global_id_to_value_table -> environment

val apply : phase * expand_env * mark * Value.t * Value.t list -> Value.t OrError.t


end
