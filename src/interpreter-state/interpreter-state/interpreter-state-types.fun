functor InterpreterStateTypesFn
(
  structure Tower : TOWER
  structure PhaseState : PHASE_STATE
) : INTERPRETER_STATE_TYPES =
struct


structure Tower = Tower
structure PhaseState = PhaseState

type phase_tower = PhaseState.t Tower.t
type phase = int

datatype t =
  State of
  { phaseTower : phase_tower
  , mkPhaseState : t * phase -> PhaseState.t
  , stdIn : TextIO.instream
  , stdOut : TextIO.outstream
  , stdErr : TextIO.outstream
  , exitRequested : bool ref
  }


end
