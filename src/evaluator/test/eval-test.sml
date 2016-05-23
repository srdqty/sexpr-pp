structure SymbolTable = SymbolTableFn(struct end)

structure Mark = MarkFn(struct end)

structure Symbol = SymbolTable.Symbol
structure SymbolMap = SymbolMapFn(Symbol)

structure LexicalContext = 
  LexicalContextFn(structure Mark = Mark
                   structure Name = Symbol)

structure ExpandEnv = ExpandEnvFn(structure LexicalContext = LexicalContext
                                  structure Symbol = Symbol
                                  structure SymbolMap = SymbolMap)

structure Value = ValueFn(structure SymbolTable = SymbolTable
                          structure LexicalContext = LexicalContext
                          structure ExpandEnv = ExpandEnv)


structure Compiler = CompilerFn(structure Value = Value
                                structure Ast = Value.Ast
                                structure SymbolMap = SymbolMap)

structure LinkTable = LinkTableFn(structure Value = Value
                                  structure SymbolMap = SymbolMap)

structure Eval = EvalFn(structure Value = Value
                        structure Ast = Value.Ast
                        structure LinkTable = LinkTable)

structure ReaderWriterDatum = ReaderWriterDatumFn(Value)

structure Reader = ReaderFn(structure DatumConstructor = ReaderWriterDatum
                            structure Lexer = Lexer)

structure Writer = WriterFn(ReaderWriterDatum)

val >>= = OrError.>>=
val >>| = OrError.>>|
infix >>=
infix >>|

val lexStream = Lexer.mkLexInstream(TextIO.getInstream(TextIO.stdIn), "stdin")
val outStream = TextIO.stdOut

val linkTable = LinkTable.mkLinkTable ()

structure V = Value
val globalFunctions =
  [
    (Symbol.fromString "cons",
     V.FunBuiltInFixArity (Symbol.fromString "cons", 2, (fn (phase, _, _, frame) => OrError.return
                                      (V.Pair(Array.sub(frame, 0),
                                              Array.sub(frame, 1))))))

   ,(Symbol.fromString "+",
     V.FunBuiltInVarArity (Symbol.fromString "+", 0, NONE, (fn (phase, _, _, frame) =>
      V.foldl (fn (x, sum) => 
                sum >>= (fn sum => 
                case x of 
                  V.Int x => OrError.return (x+sum)
                | _ => OrError.errorString ("+: argument not an integer")))
              (OrError.return 0)
              (Array.sub(frame, 0)) >>= (fn sum =>
      OrError.return (V.Int sum)))))
  ]

val () =
  List.app (fn (sym, v) => LinkTable.replace (linkTable, sym, v))
           globalFunctions

fun loop () =
  Reader.readSyntax Value.SymbolTable.table lexStream >>= (fn stx =>
  case stx of 
    NONE => OrError.return ()
  | SOME stx =>
      Compiler.compile stx >>= (fn (ast, globalSymTable) =>
      let val environment = Eval.link (globalSymTable, linkTable) in
        Eval.eval (0, ExpandEnv.mkExpandEnv(), Mark.null, environment, ast) >>= (fn value =>
        ( Writer.write print value
        ; print "\n"
        ; loop ()
        ))
      end))

val () = 
 (case loop () of
    Result.Ok () => ()
  | Result.Error err => print ((Error.toString err) ^ "\n"))
