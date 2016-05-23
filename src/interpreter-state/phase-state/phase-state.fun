functor PhaseStateFn
(
  structure Value : VALUE
  structure LinkTable : LINK_TABLE
  structure ExpandEnv : EXPAND_ENV
  sharing type ExpandEnv.t = Value.ExpandEnv.t
  sharing type ExpandEnv.LexicalContext.t = Value.LexicalContext.t
  sharing type ExpandEnv.Symbol.t = Value.SymbolTable.Symbol.t
) : PHASE_STATE =
struct


structure Value = Value
structure LinkTable = LinkTable
structure ExpandEnv = ExpandEnv

datatype t =
  PhaseState of 
  { linkTable : LinkTable.t
  , globalExpandEnv : Value.t ExpandEnv.t
  }


end
