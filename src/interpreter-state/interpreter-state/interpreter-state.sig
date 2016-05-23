signature INTERPRETER_STATE =
sig

include INTERPRETER_STATE_TYPES

val mkDefault : unit -> t

val getPhaseState : t * phase -> PhaseState.t


end
