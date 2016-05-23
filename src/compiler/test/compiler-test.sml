structure SymbolTable = SymbolTableFn(struct end)

structure Mark = MarkFn(struct end)

structure Symbol = SymbolTable.Symbol

structure LexicalContext =
  LexicalContextFn(structure Name = Symbol
                   structure Mark = Mark)

structure SymbolMap = SymbolMapFn(Symbol)

structure ExpandEnv = ExpandEnvFn(structure Symbol = Symbol
                                  structure SymbolMap = SymbolMap
                                  structure LexicalContext = LexicalContext)

structure Value = ValueFn(structure SymbolTable = SymbolTable
                          structure LexicalContext = LexicalContext
                          structure ExpandEnv = ExpandEnv)

structure Compiler = CompilerFn(structure Value = Value
                                structure Ast = Ast
                                structure SymbolMap = SymbolMap)

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

fun printConstant (outStream, v) =
  Writer.write (fn s => TextIO.output (outStream, s)) v

fun loop () =
  Reader.readSyntax Value.SymbolTable.table lexStream >>= (fn stx =>
  case stx of 
    NONE => OrError.return ()
  | SOME stx =>
      Compiler.compile stx >>= (fn (ast, linkTable) =>
      ( Ast.print (outStream, printConstant, ast)
      ; TextIO.output (outStream, "\n")
      ; loop ()
      )))

val () = 
 (case loop () of
    Result.Ok () => ()
  | Result.Error err => print ((Error.toString err) ^ "\n"))
