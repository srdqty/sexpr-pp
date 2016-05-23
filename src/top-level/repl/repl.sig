signature REPL =
sig
  structure InterpreterState : INTERPRETER_STATE

  val run : InterpreterState.t -> unit
end
