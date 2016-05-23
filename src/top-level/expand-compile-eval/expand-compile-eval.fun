functor ExpandCompileEvalFn
(
  structure ExpressionExpander : EXPANDER
  structure Reader : READER
  structure Compiler : COMPILER
  structure Eval : EVAL
  sharing type ExpressionExpander.Value.t
             = Reader.DatumConstructor.datum
  sharing type ExpressionExpander.Value.SymbolTable.t
             = Reader.DatumConstructor.symbol_table
  sharing type Eval.LinkTable.t
             = ExpressionExpander.InterpreterStateTypes.PhaseState.LinkTable.t
  sharing type Eval.Value.ExpandEnv.t
             = ExpressionExpander.Value.ExpandEnv.t
  sharing type Eval.Value.LexicalContext.Mark.t
             = ExpressionExpander.Value.LexicalContext.Mark.t
  sharing type Eval.Value.t
             = ExpressionExpander.Value.t
             = Compiler.Value.t
  sharing type Eval.Value.Ast.t
             = Compiler.Value.Ast.t
  sharing type Compiler.Value.ExpandEnv.Symbol.t
             = Eval.Value.ExpandEnv.Symbol.t
             =
             ExpressionExpander.InterpreterStateTypes.PhaseState.LinkTable.Value.ExpandEnv.Symbol.t
) : EXPAND_COMPILE_EVAL =
struct

structure ExpressionExpander = ExpressionExpander

type interpreter_state = ExpressionExpander.InterpreterStateTypes.t
type phase = int
type syntax_object = ExpressionExpander.Value.t
type value = ExpressionExpander.Value.t

structure E = ExpressionExpander
structure V = E.Value
structure S = V.SymbolTable.Symbol
structure LC = V.LexicalContext
structure Mark = LC.Mark
structure SS = E.StopSet
structure IST = ExpressionExpander.InterpreterStateTypes
structure PS = IST.PhaseState
structure Tower = IST.Tower
structure EE = V.ExpandEnv
structure C = Compiler
structure L = Eval.LinkTable

val defineName = S.fromString "define"
val defineSyntaxName = S.fromString "define-syntax"
val beginName = S.fromString "begin"
val beginForSyntaxName = S.fromString "begin-for-syntax"
val loadName = S.fromString "load"
val loadForSyntaxName = S.fromString "load-for-syntax"

fun getPhaseState (iState as IST.State {phaseTower, mkPhaseState, ...},
                   phase) =
  case Tower.get (phaseTower, phase) of
    SOME phaseState => phaseState
  | NONE =>
      let val phaseState = mkPhaseState (iState, phase)
      in
        ( Tower.set (phaseTower, phase, phaseState)
        ; phaseState)
      end

fun expandCompileEval (iState, phase, stx) = let
  val stopSet = SS.fromList 
        [ defineName
        , defineSyntaxName
        , beginName
        , beginForSyntaxName
        , loadName
        , loadForSyntaxName
        ]

  val >>= = OrError.>>=
  infix >>=

  val emptyExpandEnv = EE.mkExpandEnv ()
  
  fun locToString NONE = ":::"
    | locToString (SOME {name, line, column, ...}) =
        ( name
        ^ ":" ^ (Int.toString line)
        ^ ":" ^ (Int.toString column)
        ^ ": ")

  fun expandCompileEvalExpr (phase,
                  expr as V.Stx (V.Pair(rator as V.Stx (V.Sym ratorId, idCtx, idSrcloc),
                                         args), 
                                 ctx, srcloc)) =
        let val resolvedRatorId = LC.resolve (idCtx, ratorId, phase) in
          if S.=(defineName, resolvedRatorId) then (
            expandCompileEvalDefine (srcloc, phase, args)
          ) else if S.=(defineSyntaxName, resolvedRatorId) then (
            expandCompileEvalDefineSyntax (srcloc, phase, args)
         ) else if S.=(beginName, resolvedRatorId) then (
            expandCompileEvalBegin (srcloc, phase, args)
          ) else if S.=(beginForSyntaxName, resolvedRatorId) then (
            expandCompileEvalBegin (srcloc, phase + 1, args)
          ) else if S.=(loadName, resolvedRatorId) then (
            expandCompileEvalLoad (srcloc, phase, args)
          ) else if S.=(loadForSyntaxName, resolvedRatorId) then (
            expandCompileEvalLoad (srcloc, phase + 1, args)
          ) else (
          (*
            E.expand (iState, phase, SS.empty, expr) >>= (fn expandedExpr =>
            compileAndEval (phase, expandedExpr))
            *)
            (* Already expanded fully if no top level form was found. *)
            compileAndEval (phase, expr)
          )
        end
    | expandCompileEvalExpr (phase, expr) =
    (*
        E.expand (iState, phase, SS.empty, expr) >>= (fn expandedExpr =>
        compileAndEval (phase, expandedExpr))
        *)
        (* Already expanded fully if no top level form was found. *)
        compileAndEval (phase, expr)

  and compileAndEval (phase, expr) =
        C.compile expr >>= (fn (ast, globalSymTable) =>
        let
          val PS.PhaseState {linkTable, ...} = 
                getPhaseState (iState, phase)
          val environment = Eval.link (globalSymTable, linkTable)
        in
          if phase > 0 then (
            let
              val PS.PhaseState {globalExpandEnv, ...} =
                    getPhaseState (iState, phase-1)
            in
              Eval.eval (phase, globalExpandEnv, Mark.null, environment, ast)
            end
          ) else (
            Eval.eval (phase, emptyExpandEnv, Mark.null, environment, ast)
          )
        end)

  and expandCompileEvalDefine (_, phase, V.Pair(defineId as V.Stx(V.Sym idName, idCtx, _),
                                      V.Pair(expr, V.Nil))) =
        let
          val PS.PhaseState {globalExpandEnv, linkTable} =
                getPhaseState (iState, phase)
        in
          (*
          * Context of a global identifer is always set to empty context.
          * A more sophisticated system would rename the global identifiers
          * and mantain a list of renames to apply to each subsequent
          * expression entered into the repl. For now, just leave it as is.
          * The main concern with the current implementation is defines in
          * the output of macros at the top level will overwrite defines
          * even though they probably shouldn't because the marks are
          * would not be equal to the marks of an identifier with an
          * empty context.
          *)
          E.expand (iState, phase, SS.empty, expr) >>= (fn expandedExpr =>
          compileAndEval (phase, expandedExpr) >>= (fn value =>
          ( EE.addGlobal (globalExpandEnv, idName, EE.Var (idName, LC.empty))
          ;  L.replace (linkTable, idName, value)
          ; OrError.return V.Void)))
        end

    | expandCompileEvalDefine (srcloc, _, _) =
        OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "define: invalid syntax"))

  and expandCompileEvalDefineSyntax (_, phase, V.Pair(defineId as V.Stx(V.Sym idName, idCtx, _),
                                      V.Pair(expr, V.Nil))) =
        let
          val PS.PhaseState {globalExpandEnv, ...} =
                getPhaseState (iState, phase)
        in
          (*
          * Context of a global identifer is always set to empty context.
          * A more sophisticated system would rename the global identifiers
          * and mantain a list of renames to apply to each subsequent
          * expression entered into the repl. For now, just leave it as is.
          * The main concern with the current implementation is defines in
          * the output of macros at the top level will overwrite defines
          * even though they probably shouldn't because the marks are
          * would not be equal to the marks of an identifier with an
          * empty context.
          *)
          E.expand (iState, phase + 1, SS.empty, expr) >>= (fn expandedExpr =>
          compileAndEval (phase + 1, expandedExpr) >>= (fn value =>
          ( EE.addGlobal (globalExpandEnv, idName, EE.Val value)
          ; OrError.return V.Void)))
        end

    | expandCompileEvalDefineSyntax (srcloc, _, _) =
        OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "define-syntax: invalid syntax"))


  and expandCompileEvalBegin (_, phase, V.Pair(expr, V.Nil)) =
        expandCompileEval (iState, phase, expr)

    | expandCompileEvalBegin (srcloc, phase, V.Pair(expr, rest)) =
        expandCompileEval (iState, phase, expr) >>= (fn _ =>
        expandCompileEvalBegin (srcloc, phase, rest))
    
    | expandCompileEvalBegin (_, phase, V.Nil) = OrError.return V.Void

    | expandCompileEvalBegin (srcloc, _, _) =
        OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "begin: invalid syntax"))
  
  
  and expandCompileEvalLoad (srcloc, phase, V.Pair(V.Stx(V.Str filename, _, _), V.Nil)) =
        let
          val fileStream = (OrError.return (TextIO.openIn filename))
            handle (IO.Io _) =>
              OrError.errorThunk (fn () =>
                ( (locToString srcloc)
                ^ "load: cannot open file: "
                ^ filename))
        in
          fileStream >>= (fn fileStream =>
          let
            val lexStream = Reader.Lexer.mkLexInstream
                              (TextIO.getInstream fileStream, filename)
            fun loop () =
              Reader.readSyntax V.SymbolTable.table lexStream >>= (fn stx =>
              case stx of
                NONE => 
                  ( TextIO.closeIn fileStream
                  ; OrError.return V.Void)
              | SOME form =>
                  expandCompileEval (iState, phase, form) >>= (fn _ =>
                  loop ()))
          in
            loop ()
          end)
        end

    | expandCompileEvalLoad (srcloc, _, _) =
        OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "load: invalid syntax"))
in
  E.expand (iState, phase, stopSet, stx) >>= (fn partialExpand =>
  expandCompileEvalExpr (phase, partialExpand))
end


end
