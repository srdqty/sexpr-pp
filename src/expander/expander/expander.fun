functor ExpanderFn
(
  structure Value : VALUE
  structure InterpreterStateTypes : INTERPRETER_STATE_TYPES
  structure StopSet : STOP_SET
  structure Compiler : COMPILER
  structure Eval : EVAL
  sharing type Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.ExpandEnv.Symbol.t
             = InterpreterStateTypes.PhaseState.LinkTable.Value.SymbolTable.Symbol.t
             = Compiler.Value.SymbolTable.Symbol.t
             = Eval.Value.SymbolTable.Symbol.t
             = StopSet.Symbol.t
  sharing type Value.LexicalContext.t
             = InterpreterStateTypes.PhaseState.Value.LexicalContext.t
             = InterpreterStateTypes.PhaseState.ExpandEnv.LexicalContext.t
  sharing type InterpreterStateTypes.PhaseState.LinkTable.t
             = Eval.LinkTable.t
  sharing type Value.t
             = Eval.Value.t
             = InterpreterStateTypes.PhaseState.Value.t
             = Compiler.Value.t
  sharing type Value.Ast.t
             = Eval.Value.Ast.t
             = InterpreterStateTypes.PhaseState.Value.Ast.t
             = Compiler.Value.Ast.t
  sharing type InterpreterStateTypes.PhaseState.Value.LexicalContext.Mark.t
             = Value.LexicalContext.Mark.t
             = Eval.Value.LexicalContext.Mark.t
  sharing type InterpreterStateTypes.PhaseState.Value.ExpandEnv.t
             = Value.ExpandEnv.t
             = Eval.Value.ExpandEnv.t
) : EXPANDER =
struct


structure Value = Value
structure InterpreterStateTypes = InterpreterStateTypes
structure StopSet = StopSet

type syntax_object = Value.t
type phase = int

structure V = Value
structure LC = V.LexicalContext
structure S = V.SymbolTable.Symbol
structure IST = InterpreterStateTypes
structure PS = IST.PhaseState
structure Tower = IST.Tower
structure E = PS.ExpandEnv
structure SS = StopSet

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


fun expand (iState, phase, stopSet, stx) = let
  val >>= = OrError.>>=
  infix >>=

  fun locToString NONE = ":::"
    | locToString (SOME {name, line, column, position, span}) =
        ( name
        ^ ":" ^ (Int.toString line)
        ^ ":" ^ (Int.toString column)
        ^ ": ")
      

  fun checkDuplicate (id, srcloc, ids) = OrError.return ()
    (*
    if V.exists (fn x => S.=(id,x)) ids then (
      OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "lambda: duplicate parameter name: "
        ^ (S.toString id)
        ))
    ) else (
      OrError.return ()
    )
    *)

  (*
  * Let (Stx <symbol>) represent a symbol wrapped in syntax.
  * Lists of ids will look like one of these cases:
  * Zero arg: (Stx ())
  * Fixed list of args: (Stx ((Stx <symbol>) ...))
  * Var arity >= 0 args: (Stx <symbol>)
  * Var arity >= n args: (Stx ((Stx <symbol>) ... . (Stx <symbol>)))
  *)

      (* This case is the case of Nil at the end of fixed list of args *)
  fun renameParams (_, env, renames, renamedParams, V.Nil) = 
        OrError.return (env, renames, V.reverse renamedParams)

      (* This case is the zero arg case *)
    | renameParams (_, env, renames, renamedParams, (V.Stx (V.Nil, _, _))) =
        OrError.return (env, renames, V.reverse renamedParams)

      (* This case is first pair of fixed list of args *)
    | renameParams (srcloc, env, renames, renamedParams,
                          (V.Stx (V.Pair (V.Stx (V.Sym id, ctx, idSrcloc), 
                                          tl), _, _))) =
        let
          val newName = S.freshSym (S.toString id)
          val rename = { renamePhase = phase, renameCtx = ctx,
                         oldName = id, newName = newName }
          val newCtx = LC.rename (ctx, rename)
          val newId = V.Stx (V.Sym id, newCtx, idSrcloc)
          val transform = E.Var (id, newCtx)
        in
          checkDuplicate (newId, idSrcloc, renamedParams) >>= (fn () => 
          renameParams (srcloc,
                        E.extend (env, newName, transform),
                        rename :: renames,
                        V.Pair(newId, renamedParams),
                        tl))
        end

      (* This case is inner pair of fixed list of args *)
    | renameParams (srcloc, env, renames, renamedParams,
                          V.Pair (V.Stx (V.Sym id, ctx, idSrcloc), 
                                  tl)) =
         let
          val newName = S.freshSym (S.toString id)
          val rename = { renamePhase = phase, renameCtx = ctx,
                         oldName = id, newName = newName }
          val newCtx = LC.rename (ctx, rename)
          val newId = V.Stx (V.Sym id, newCtx, idSrcloc)
          val transform = E.Var (id, newCtx)
        in
          checkDuplicate (newId, idSrcloc, renamedParams) >>= (fn () => 
          renameParams (srcloc,
                        E.extend (env, newName, transform),
                        rename :: renames,
                        V.Pair(newId, renamedParams),
                        tl))
        end

      (* This case is the variable arity id case, either at
      *  the end of a list or a single identifier *)
    | renameParams (_, env, renames, renamedParams, 
                       V.Stx (V.Sym id, ctx, idSrcloc)) =
        let
          val newName = S.freshSym (S.toString id)
          val rename = { renamePhase = phase, renameCtx = ctx,
                         oldName = id, newName = newName }
          val newCtx = LC.rename (ctx, rename)
          val newId = V.Stx (V.Sym id, newCtx, idSrcloc)
          val transform = E.Var (id, newCtx)
        in
          checkDuplicate (newId, idSrcloc, renamedParams) >>= (fn () => 
          OrError.return (E.extend (env, newName, transform),
                          rename :: renames,
                          V.reversePrepend (renamedParams, newId)))
        end

      (* TODO: add more cases and make error message more specific *)
    | renameParams (srcloc, _, _, _, _) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "lambda: invalid parameter list"
          ))

  fun expandQuote (quoteExpr as (V.Stx(V.Pair(quoteId,
                                        V.Pair(stx, V.Nil)), ctx, srcloc))) =
        OrError.return quoteExpr
    | expandQuote (V.Stx(_, _, srcloc)) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "quote: invalid syntax"
          ))
    | expandQuote _ =
        OrError.errorString "quote: invalid syntax"
 

  fun expandExpr (env, stx as V.Stx (V.Sym id, ctx, srcloc)) =
        let val resolvedId = LC.resolve (ctx, id, phase) in
          if SS.member (stopSet, resolvedId) then (
            OrError.return stx
          ) else (
            case E.lookup (env, resolvedId) of
              SOME (E.Var (sym, newCtx)) =>
                OrError.return (V.Stx (V.Sym sym, newCtx, srcloc))
            | SOME (E.Val macro) =>
                expandByMacro (env, macro, stx)
            | SOME E.Fun =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "lambda: invalid syntax"
                  ))
            | SOME E.Quote =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "quote: invalid syntax"
                  ))
            | SOME E.If =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "if: invalid syntax"
                  ))
            | SOME E.Begin =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "begin: invalid syntax"
                  ))
            | SOME E.Letrec =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "letrec: invalid syntax"
                  ))
            | SOME E.LetSyntax =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "let-syntax: invalid syntax"
                  ))
            | NONE =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "undefined identifier: "
                  ^ (S.toString id)
                  ))
          )
        end

    | expandExpr (env, stx as V.Stx (V.Pair(V.Stx (V.Sym id, ctx, srcloc),
                                            _), 
                                    _, _)) =
        let val resolvedId = LC.resolve (ctx, id, phase) in
          if SS.member (stopSet, resolvedId) then (
            OrError.return stx
          ) else (
            case E.lookup (env, resolvedId) of
              SOME E.Fun => expandLambda (env, stx)
            | SOME E.Quote => expandQuote stx
            | SOME E.If => expandIf (env, stx)
            | SOME E.Begin => expandBegin (env, stx)
            | SOME E.Letrec => expandLetrec (env, stx)
            | SOME E.LetSyntax => expandLetSyntax (env, stx)
            | SOME (E.Val macro) => expandByMacro (env, macro, stx)
            | SOME (E.Var _) => expandApp (env, stx)
            | NONE =>
                OrError.errorThunk (fn () =>
                  ( (locToString srcloc)
                  ^ "undefined identifier: "
                  ^ (S.toString id)
                  ))
        )
      end
    
    | expandExpr (env, stx as V.Stx(V.True, _, _)) = 
        OrError.return stx
    | expandExpr (env, stx as V.Stx(V.False, _, _)) = 
        OrError.return stx
    | expandExpr (env, stx as V.Stx(V.Int _, _, _)) = 
        OrError.return stx
    | expandExpr (env, stx as V.Stx(V.Char _, _, _)) = 
        OrError.return stx
    | expandExpr (env, stx as V.Stx(V.Str _, _, _)) = 
        OrError.return stx
    
    | expandExpr (env, stx as V.Stx (V.Pair(_, _), _, _)) =
        expandApp (env, stx)
    
    | expandExpr (env, stx) =
        ( print "here\n"
;        OrError.errorString "invalid syntax"
)

  and expandByMacro (env, macro, stx) =  
    let
      val mark = LC.Mark.freshMark ()
      fun applyMark stx =
        V.ctxMap (fn ctx => LC.mark (ctx, mark)) stx
    in
      applyMark stx >>= (fn markedStx =>
      Eval.apply (phase+1, env, mark, macro, [markedStx]) 
  >>= (fn macroResult =>
      applyMark macroResult >>= (fn markedResult =>
      expandExpr (env, markedResult))))
    end

  and expandApp (env, V.Stx(lst, ctx, srcloc)) =
        expandList ("application: invalid syntax", srcloc, V.Nil,
                     env, lst)
        >>= (fn expandedLst => 
              OrError.return (V.Stx(expandedLst, ctx, srcloc)))
    | expandApp (_, _) =
        OrError.errorString "application: invalid syntax"
          

  and expandList (_, _, output, _, V.Nil) = 
        OrError.return (V.reverse output)
    | expandList (msg, srcloc, output, env, V.Pair(hd, tl)) =
        expandExpr (env, hd) >>= (fn expr =>
        expandList (msg, srcloc, V.Pair(expr, output), env, tl))
    | expandList (errorMsg, srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ errorMsg
            ))
 
  and expandLambda (env, V.Stx (V.Pair(lambdaId,
                                       V.Pair(params as V.Stx(_, pCtx, pSrcloc),
                                              body as V.Pair(_,_))),
                                ctx, srcloc)) =
        renameParams (srcloc, env, [], V.Nil, params)
        >>= (fn (newEnv, renames, expandedParams) =>
              let 
                fun applyRenames stx =
                  V.ctxMap (fn ctx => LC.applyRenames (ctx, renames)) stx
                fun f (stx, lst) =
                  lst >>= (fn lst =>
                  applyRenames stx >>= (fn renamedStx =>
                  OrError.return (V.Pair (renamedStx, lst))))
                fun renameBody body = 
                  V.foldr f (OrError.return V.Nil) body
              in
                renameBody body >>= (fn renamedBody =>
                expandList ("lambda: invalid syntax", 
                            srcloc, V.Nil, newEnv, renamedBody))
              end
        >>= (fn expandedBody =>
              OrError.return (V.Stx (V.Pair(lambdaId,
                                            V.Pair(V.Stx (expandedParams,
                                                          pCtx,
                                                          pSrcloc),
                                                   expandedBody)),
                                     ctx,
                                     srcloc))))

    | expandLambda (_, V.Stx (_, _, srcloc)) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "lambda: invalid syntax"
            ))

    | expandLambda (_, _) =
        OrError.errorString "lambda: invalid syntax"


  and extractBinding (_, env,
                      V.Stx(V.Pair(V.Stx(V.Sym sym, ctx, idSrcloc),
                                   V.Pair(expr, V.Nil)), _, _),
                      renamedIds) =
        let
          val newName = S.freshSym (S.toString sym)
          val rename = { renamePhase = phase, renameCtx = ctx,
                         oldName = sym, newName = newName }
          val newCtx = LC.rename (ctx, rename)
          val newId = V.Stx (V.Sym sym, newCtx, idSrcloc)
          val transform = E.Var (sym, newCtx)
        in
          (*checkDuplicate (resolvedId, idSrcloc, renamedIds) >>= (fn () =>*)
          OrError.return (E.extend (env, newName, transform), 
                          rename, 
                          newId,
                          expr)
        end
    | extractBinding (srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax (not an identifier and expression for a binding)"
            ))
        

  and extractBindings (_, env, renames, ids, exprs, V.Nil) = 
        OrError.return (env, renames, ids, exprs)
    | extractBindings (srcloc, env, renames, ids, exprs,
                       V.Pair(binding, rest)) =
        extractBinding (srcloc, env, binding, ids) 
    >>= (fn (newEnv, rename, id, expr) =>
        extractBindings (srcloc, newEnv, rename :: renames,
                         V.Pair (id, ids), V.Pair(expr, exprs),
                         rest))
    | extractBindings (srcloc, _, _, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax"
            ))

  and expandLetrecExprs (env, renames, output, V.Nil) = OrError.return output
    | expandLetrecExprs (env, renames, output, V.Pair (expr, rest)) =
        let
          fun renameExpr expr = 
            V.ctxMap (fn ctx => LC.applyRenames (ctx, renames)) expr
        in
          renameExpr expr >>= (fn renamedExpr =>
          expandExpr (env, renamedExpr) >>= (fn expandedExpr =>
          expandLetrecExprs (env, renames,
                             V.Pair (expandedExpr, output), rest)))
        end
    | expandLetrecExprs (env, _, _, _) =
        OrError.errorString "expandLetrecExprs: internal error"


  and mkExpandedBindings (V.Nil, V.Nil, output) = OrError.return output
    | mkExpandedBindings (V.Pair(id, ids), V.Pair(expr, exprs), output) =
        let
          val expandedBinding =
            V.Stx (V.Pair(id, 
                          V.Pair (expr, V.Nil)),
                   LC.empty, NONE)
        in
          mkExpandedBindings (ids, exprs, V.Pair (expandedBinding, output))
        end
    | mkExpandedBindings (_, _, _) = 
        OrError.errorString "mkExpandedBindings: internal error"
 
  
  and expandLetrec (env, V.Stx (V.Pair(letrecId,
                                 V.Pair(V.Stx(bindings, bindingsCtx, 
                                              bindingsSrcloc),
                                 body as V.Pair(_,_))),
                                 ctx, srcloc)) =
        extractBindings (srcloc, env, [], V.Nil, V.Nil, bindings)
    >>= (fn (newEnv, renames, expandedIds, exprs) =>
        expandLetrecExprs (newEnv, renames, V.Nil, exprs)
    >>= (fn expandedExprs =>
        mkExpandedBindings (expandedIds, V.reverse expandedExprs, V.Nil)
    >>= (fn expandedBindings =>
          let
            fun applyRenames stx =
              V.ctxMap (fn ctx => LC.applyRenames (ctx, renames)) stx
            fun f (stx, lst) =
              lst >>= (fn lst =>
              applyRenames stx >>= (fn renamedStx =>
              OrError.return (V.Pair (renamedStx, lst))))
            fun renameBody body = 
              V.foldr f (OrError.return V.Nil) body
          in
            renameBody body >>= (fn renamedBody =>
            expandList ("letrec: invalid syntax",
                        srcloc, V.Nil, newEnv, renamedBody))
          end
    >>= (fn expandedBody =>
        OrError.return (V.Stx (V.Pair(letrecId,
                               V.Pair(V.Stx(expandedBindings, bindingsCtx,
                                            bindingsSrcloc),
                               expandedBody)), ctx, srcloc))))))

    | expandLetrec (_, V.Stx (_, _, srcloc)) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax"
            ))
    | expandLetrec (_, _) =
        OrError.errorString "letrec: invalid syntax"

 
  and evalMacro (_, env,
                      V.Stx(V.Pair(V.Stx(V.Sym sym, ctx, idSrcloc),
                                   V.Pair(expr, V.Nil)), _, _),
                      renamedIds) =
        let 
          val newName = S.freshSym (S.toString sym)
          val rename = { renamePhase = phase, renameCtx = ctx,
                         oldName = sym, newName = newName }
          val newCtx = LC.rename (ctx, rename)
          val newId = V.Stx (V.Sym sym, newCtx, idSrcloc)
        in
          (*checkDuplicate (resolvedId, idSrcloc, renamedIds) >>= (fn () =>*)
          expand (iState, phase + 1, StopSet.empty, expr)
      >>= (fn expandedMacro =>
          Compiler.compile expandedMacro >>= (fn (ast, idxToIdTable) =>
          let
            val PS.PhaseState {linkTable, ...} =
                  getPhaseState (iState, phase + 1)
            val evalEnv = Eval.link (idxToIdTable, linkTable)
          in
            (* what about the current mark? *)
            Eval.eval (phase+1, env, LC.Mark.null, evalEnv, ast)
        >>= (fn macroValue =>
            OrError.return (E.extend (env, newName, E.Val macroValue),
                            rename,
                            newId))
          end))
        end
    | evalMacro (srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "let-syntax: invalid syntax (not an identifier and expression for a binding)"
            ))


  and evalMacros (_, env, renames, ids, V.Nil) = 
        OrError.return (env, renames, ids)
    | evalMacros (srcloc, env, renames, ids, V.Pair(binding, rest)) =
        evalMacro (srcloc, env, binding, ids) >>= (fn (newEnv, rename, id) =>
        evalMacros (srcloc, newEnv, rename :: renames, V.Pair (id, ids), rest))
    | evalMacros (srcloc, _, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "let-syntax: invalid syntax"
            ))


  and expandLetSyntax (env, V.Stx (V.Pair(letSyntaxId,
                                 V.Pair(V.Stx(bindings, bindingsCtx, 
                                              bindingsSrcloc),
                                 body as V.Pair(_,_))),
                                 ctx, srcloc)) =
        evalMacros (srcloc, env, [], V.Nil, bindings)
    >>= (fn (newEnv, renames, expandedIds) =>
          let
            fun applyRenames stx =
              V.ctxMap (fn ctx => LC.applyRenames (ctx, renames)) stx
            fun f (stx, lst) =
              lst >>= (fn lst =>
              applyRenames stx >>= (fn renamedStx =>
              OrError.return (V.Pair (renamedStx, lst))))
            fun renameBody body = 
              V.foldr f (OrError.return V.Nil) body
          in
            renameBody body >>= (fn renamedBody =>
            expandList ("let-syntax: invalid syntax",
                        srcloc, V.Nil, newEnv, renamedBody))
          end
    >>= (fn expandedBody =>
        OrError.return (V.Stx (V.Pair(V.Stx (V.Sym (S.fromString "begin"), LC.empty, NONE),
                                      expandedBody), ctx, srcloc))))

    | expandLetSyntax (_, V.Stx (_, _, srcloc)) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "let-syntax: invalid syntax"
            ))
    | expandLetSyntax (_, _) =
        OrError.errorString "let-syntax: invalid syntax"


  and expandIf (env, V.Stx(V.Pair(ifId,
                                  V.Pair(cond, 
                                  V.Pair(thn,
                                  V.Pair(els, V.Nil)))), ctx, srcloc)) =
        expandExpr (env, cond) >>= (fn expandedCond =>
        expandExpr (env, thn) >>= (fn expandThen =>
        expandExpr (env, els) >>= (fn expandedElse =>
        OrError.return (V.Stx (V.Pair(ifId,
                               V.Pair(expandedCond,
                               V.Pair(expandThen,
                               V.Pair(expandedElse, V.Nil)))), ctx, srcloc)))))
    | expandIf (_, V.Stx(_, _, srcloc)) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "if: invalid syntax"
          ))
    | expandIf (_, _) = OrError.errorString "if: invalid syntax"


  and expandBegin (env, V.Stx (V.Pair(beginId,
                                      body as V.Pair(_, _)), ctx, srcloc)) =
        expandList ("begin: invalid syntax", srcloc,
                     V.Nil, env, body)
        >>= (fn expandedBody =>
              OrError.return (V.Stx (V.Pair(beginId, expandedBody),
                                     ctx, srcloc)))
    | expandBegin (_, V.Stx (V.Pair(beginId, V.Nil), _, srcloc)) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "begin: empty form is not allowed"
            ))
    | expandBegin (env, V.Stx(_, _, srcloc)) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "begin: invalid syntax"
            ))
    | expandBegin (_, _) = OrError.errorString "begin: invalid syntax"

  val PS.PhaseState {globalExpandEnv, ...} =
        getPhaseState (iState, phase)
in
  expandExpr (globalExpandEnv, stx)
end


end
