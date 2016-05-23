signature CORE_FUNCTIONS =
sig


  structure Value : VALUE
  structure InterpreterStateTypes : INTERPRETER_STATE_TYPES
  (*
  sharing type Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.
             *)


  val mkCoreFunctions :
    InterpreterStateTypes.t -> (Value.SymbolTable.Symbol.t * Value.t) list


end
