structure SymbolTable = SymbolTableFn(struct end)
structure Mark = MarkFn(struct end)
structure LexicalContext = 
  LexicalContextFn(structure Mark = Mark
                   structure Name = SymbolTable.Symbol)
structure Value = ValueFn(structure SymbolTable = SymbolTable
                          structure LexicalContext = LexicalContext)
structure SymbolMap = SymbolMapFn(Value.SymbolTable.Symbol)
structure Compiler = CompilerFn(structure Value = Value
                                structure Ast = Ast
                                structure SymbolMap = SymbolMap)
structure LinkTable = LinkTableFn(structure Value = Value
                                  structure SymbolMap = SymbolMap)
structure Eval = EvalFn(structure Value = Value
                        structure Ast = Ast
                        structure LinkTable = LinkTable)
structure ReaderWriterDatum = ReaderWriterDatumFn(Value)
structure Reader = ReaderFn(structure DatumConstructor = ReaderWriterDatum
                            structure Lexer = Lexer)
structure Writer = WriterFn(ReaderWriterDatum)

(*
structure TopLevelAst = TopLevelAstFn(Value)

structure TopLevelCompiler = 
    TopLevelCompilerFn(structure Value = Value
                       structure TopLevelAst = TopLevelAst
                       structure ExpressionCompiler = Compiler
                       structure Reader = Reader)
                       *)
structure ExpandEnv = ExpandEnvFn(structure Value = Value
                                  structure SymbolMap = SymbolMap)
structure PhaseState = PhaseStateFn(structure LinkTable = LinkTable
                                    structure ExpandEnv = ExpandEnv
                                    )
structure InterpreterStateTypes =
  InterpreterStateTypesFn(structure Tower = Tower
                          structure PhaseState = PhaseState)
structure CoreFunctions =
  CoreFunctionsFn(structure Value = Value
                  structure InterpreterStateTypes = InterpreterStateTypes
                  structure Eval = Eval
                  structure Writer = Writer)
structure InterpreterState =
  InterpreterStateFn(structure InterpreterStateTypes = InterpreterStateTypes
                     structure CoreFunctions = CoreFunctions)
                                  (*
structure TopLevelEval = 
  TopLevelEvalFn(structure TopLevelAst = TopLevelAst
                 structure ExpressionEval = Eval
                 structure PhaseState = PhaseState
                 structure InterpreterState = InterpreterState)
                 *)

structure StopSet = StopSetFn (SymbolTable.Symbol)
structure Expander = ExpanderFn(structure Value = Value
                                structure InterpreterState = InterpreterState
                                structure StopSet = StopSet
                                structure Compiler = Compiler
                                structure Eval = Eval)

val >>= = OrError.>>=
infix >>=

structure IS = InterpreterState
structure S = SymbolTable.Symbol

val iState = IS.mkDefault ()
val IS.State {stdIn, stdOut, exitRequested, ...} = iState

val lexStream = Lexer.mkLexInstream(TextIO.getInstream stdIn, "top-level")

fun output str = TextIO.output (stdOut, str)

fun write Value.Void = ()
  | write value = (Writer.writeRenamedSyntax output value; output "\n")

val stopSet = StopSet.fromList
  [ S.fromString "stop1"
  , S.fromString "stop2"
  , S.fromString "stop3"
  ]

fun rep () =
  ( output ">> "
  ; TextIO.flushOut stdOut
  ; Reader.readSyntax Value.SymbolTable.table lexStream >>= (fn stx =>
    case stx of 
      NONE => OrError.return false
    | SOME stx =>
        Expander.expand (iState, 0, stopSet, stx) >>= (fn expandedStx =>
        ( write expandedStx; OrError.return true))))

fun loop () =
  case rep () of
    Result.Ok true => 
      if !exitRequested then (
        ()
      ) else (
        loop ()
      )
  | Result.Ok false => output "\n"
  | Result.Error err =>
      ( output ("Error: " ^ (Error.toString err) ^ "\n")
      ; loop ())

val _ = loop ()
