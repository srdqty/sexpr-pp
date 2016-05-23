structure SymbolTable = SymbolTableFn(struct end)
structure SymbolMap = SymbolMapFn(SymbolTable.Symbol)
structure StopSet = StopSetFn(SymbolTable.Symbol)
structure Mark = MarkFn(struct end)
structure LexicalContext = 
  LexicalContextFn(structure Mark = Mark
                   structure Name = SymbolTable.Symbol)
structure ExpandEnv = ExpandEnvFn(structure LexicalContext = LexicalContext
                                  structure Symbol = SymbolTable.Symbol
                                  structure SymbolMap = SymbolMap)
structure Value = ValueFn(structure SymbolTable = SymbolTable
                          structure LexicalContext = LexicalContext
                          structure ExpandEnv = ExpandEnv)
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

structure PhaseState = PhaseStateFn(structure LinkTable = LinkTable
                                    structure ExpandEnv = ExpandEnv
                                    structure Value = Value
                                    )
structure InterpreterStateTypes =
  InterpreterStateTypesFn(structure Tower = Tower
                          structure PhaseState = PhaseState)
structure Expander =
  ExpanderFn(structure Value = Value
             structure InterpreterStateTypes = InterpreterStateTypes
             structure Compiler = Compiler
             structure Eval = Eval
             structure StopSet = StopSet)
structure CoreFunctions =
  CoreFunctionsFn(structure Value = Value
                  structure InterpreterStateTypes = InterpreterStateTypes
                  structure Eval = Eval
                  structure Expander = Expander
                  structure Writer = Writer)
structure InterpreterState =
  InterpreterStateFn(structure InterpreterStateTypes = InterpreterStateTypes
                     structure CoreFunctions = CoreFunctions)

val >>= = OrError.>>=
infix >>=

structure IS = InterpreterState
structure PS = IS.PhaseState

val iState = IS.mkDefault ()
val IS.State {stdIn, stdOut, exitRequested, ...} = iState

val lexStream = Lexer.mkLexInstream(TextIO.getInstream stdIn, "top-level")

fun output str = TextIO.output (stdOut, str)

fun write Value.Void = ()
  | write value = (Writer.write output value; output "\n")

val dummyExpEnv = ExpandEnv.mkExpandEnv ()

fun rep () =
  ( output ">> "
  ; TextIO.flushOut stdOut
  ; Reader.readSyntax Value.SymbolTable.table lexStream >>= (fn stx =>
    case stx of 
      NONE => OrError.return false
    | SOME stx =>
        Expander.expand (iState, 0, StopSet.empty, stx) >>= (fn expandedStx =>
        Compiler.compile expandedStx >>= (fn (ast, globalSymTable) =>
        let 
          val PS.PhaseState {linkTable, ...} =
              IS.getPhaseState (iState, 0)
          val environment = Eval.link (globalSymTable, linkTable)
        in
          Eval.eval (0, dummyExpEnv, Mark.null, environment, ast)
        end
    >>= (fn value =>
        ( write value
        ; OrError.return true))))))

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
