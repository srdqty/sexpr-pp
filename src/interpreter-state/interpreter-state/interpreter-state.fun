functor InterpreterStateFn
(
  structure InterpreterStateTypes : INTERPRETER_STATE_TYPES
  structure CoreFunctions : CORE_FUNCTIONS
  sharing type CoreFunctions.Value.t
             = InterpreterStateTypes.PhaseState.LinkTable.Value.t
  sharing type CoreFunctions.Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.LinkTable.Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.Value.LexicalContext.Name.t
  sharing type InterpreterStateTypes.t
             = CoreFunctions.InterpreterStateTypes.t
  sharing type InterpreterStateTypes.PhaseState.ExpandEnv.LexicalContext.t
             = InterpreterStateTypes.PhaseState.Value.LexicalContext.t
) : INTERPRETER_STATE =
struct

open InterpreterStateTypes

fun getPhaseState (iState as State {phaseTower, mkPhaseState, ...},
                   phase) =
  case Tower.get (phaseTower, phase) of
    SOME phaseState => phaseState
  | NONE =>
      let val phaseState = mkPhaseState (iState, phase)
      in
        ( Tower.set (phaseTower, phase, phaseState)
        ; phaseState)
      end

structure S = CoreFunctions.Value.SymbolTable.Symbol
structure E = PhaseState.ExpandEnv
structure V = PhaseState.Value
structure LC = V.LexicalContext

val coreFormExpandEnv =
  [ (S.fromString "lambda", E.Fun)
  , (S.fromString "quote", E.Quote)
  , (S.fromString "syntax", E.Quote)
  , (S.fromString "if", E.If)
  , (S.fromString "begin", E.Begin)
  , (S.fromString "letrec", E.Letrec)
  , (S.fromString "let-syntax", E.LetSyntax)
  ]

fun genCoreFunctionsExpandEnv ([], output) = output
  | genCoreFunctionsExpandEnv ((sym, _) :: rest, output) =
      genCoreFunctionsExpandEnv 
          (rest, (sym, E.Var (sym, LC.empty)) :: output)

fun mkDefault () = let
  val phaseTower = Tower.mkTower ()
  fun mkPhaseState (iState, i) = let
    val coreFunctions = CoreFunctions.mkCoreFunctions iState
    val expandEnvAssocList =
          List.revAppend (coreFormExpandEnv,
                          genCoreFunctionsExpandEnv (coreFunctions, []))
    val expandEnv = E.fromList expandEnvAssocList
  in
    PhaseState.PhaseState
      { linkTable = PhaseState.LinkTable.fromList coreFunctions
      , globalExpandEnv = expandEnv
      }
  end
in
  State { phaseTower = phaseTower
        , mkPhaseState = mkPhaseState
        , stdIn = TextIO.stdIn
        , stdOut = TextIO.stdOut
        , stdErr = TextIO.stdErr
        , exitRequested = ref false
        }
end


end
