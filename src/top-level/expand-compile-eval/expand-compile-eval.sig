signature EXPAND_COMPILE_EVAL =
sig

structure ExpressionExpander : EXPANDER

type interpreter_state = ExpressionExpander.InterpreterStateTypes.t
type phase = int
type syntax_object = ExpressionExpander.Value.t
type value = ExpressionExpander.Value.t

val expandCompileEval : 
  interpreter_state * phase * syntax_object -> value OrError.t


end
