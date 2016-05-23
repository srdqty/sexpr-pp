signature EXPANDER =
sig


structure Value : VALUE
structure InterpreterStateTypes : INTERPRETER_STATE_TYPES
structure StopSet : STOP_SET
  
sharing type Value.SymbolTable.Symbol.t
           = InterpreterStateTypes.PhaseState.Value.SymbolTable.Symbol.t
           = InterpreterStateTypes.PhaseState.ExpandEnv.Symbol.t
           = InterpreterStateTypes.PhaseState.LinkTable.Value.SymbolTable.Symbol.t
           = StopSet.Symbol.t
sharing type Value.LexicalContext.t
           = InterpreterStateTypes.PhaseState.Value.LexicalContext.t
           = InterpreterStateTypes.PhaseState.ExpandEnv.LexicalContext.t
sharing type Value.t
           = InterpreterStateTypes.PhaseState.Value.t
sharing type Value.Ast.t
           = InterpreterStateTypes.PhaseState.Value.Ast.t
sharing type InterpreterStateTypes.PhaseState.Value.LexicalContext.Mark.t
           = Value.LexicalContext.Mark.t
sharing type InterpreterStateTypes.PhaseState.Value.ExpandEnv.t
           = Value.ExpandEnv.t

type syntax_object = Value.t
type phase = int

val expand : InterpreterStateTypes.t * phase * StopSet.t * syntax_object -> 
             syntax_object OrError.t


end
