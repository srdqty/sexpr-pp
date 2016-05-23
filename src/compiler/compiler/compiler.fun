functor CompilerFn
(
  structure Value : VALUE
  structure SymbolMap : SYMBOL_MAP
  sharing type Value.SymbolTable.Symbol.t = SymbolMap.Symbol.t
)
  : COMPILER =
struct


structure Value = Value
structure CompileEnv = CompileEnvFn(Value.SymbolTable.Symbol)
structure IdToIndexMap = IdToIndexMapFn(SymbolMap)

type syntax_object = Value.t
type ast = Value.t Value.Ast.t
type index_to_global_id_table = Value.SymbolTable.Symbol.t vector

structure V = Value
structure LC = V.LexicalContext
structure A = Value.Ast
structure CE = CompileEnv
structure S = V.SymbolTable.Symbol
structure I = IdToIndexMap

fun compile stx = let
  val >>= = OrError.>>=
  infix >>=

  val idMap = I.mkMap ()

  fun locToString NONE = ":::"
    | locToString (SOME {name, line, column, ...}) =
        ( name
        ^ ":" ^ (Int.toString line)
        ^ ":" ^ (Int.toString column)
        ^ ": ")
      

  fun checkDuplicate (id, srcloc, ids) =
    if List.exists (fn x => S.=(id,x)) ids then (
      OrError.errorThunk (fn () =>
        ( (locToString srcloc)
        ^ "lambda: duplicate parameter name: "
        ^ (S.toString id)
        ))
    ) else (
      OrError.return ()
    )

  datatype params_type =
    FixArity of int * S.t list
  | VarArity of int * S.t list

  (*
  * Let (Stx <symbol>) represent a symbol wrapped in syntax.
  * Lists of ids will look like one of these cases:
  * Zero arg: (Stx ())
  * Fixed list of args: (Stx ((Stx <symbol>) ...))
  * Var arity >= 0 args: (Stx <symbol>)
  * Var arity >= n args: (Stx ((Stx <symbol>) ... . (Stx <symbol>)))
  *)

      (* This case is the case of Nil at the end of fixed list of args *)
  fun paramsToEnvList (_, output, count, V.Nil) = 
        OrError.return (FixArity (count, List.rev output))

      (* This case is the zero arg case *)
    | paramsToEnvList (_, output, count, (V.Stx (V.Nil, _, _))) =
        OrError.return (FixArity (count, List.rev output))

      (* This case is first pair of fixed list of args *)
    | paramsToEnvList (srcloc, output, count,
                          (V.Stx (V.Pair (V.Stx (V.Sym id, ctx, id_srcloc), 
                                          tl), _, _))) =
         let
          val resolvedId = LC.resolve (ctx, id, 0)
        in
          checkDuplicate (resolvedId, id_srcloc, output) >>= (fn () => 
          paramsToEnvList (srcloc, resolvedId :: output, count + 1, tl))
        end

      (* This case is inner pair of fixed list of args *)
    | paramsToEnvList (srcloc, output, count,
                          V.Pair (V.Stx (V.Sym id, ctx, id_srcloc), 
                                  tl)) =
        let
          val resolvedId = LC.resolve (ctx, id, 0)
        in
          checkDuplicate (resolvedId, id_srcloc, output) >>= (fn () => 
          paramsToEnvList (srcloc, resolvedId :: output, count + 1, tl))
        end

      (* This case is the variable arity id case, either at
      *  the end of a list or a single identifier *)
    | paramsToEnvList (_, output, count, V.Stx (V.Sym id, ctx, id_srcloc)) =
        let
          val resolvedId = LC.resolve (ctx, id, 0)
        in
          (* Count is the minimum number of arguments for variable
          * arity functions *)
          checkDuplicate (resolvedId, id_srcloc, output) >>= (fn () => 
          OrError.return (VarArity (count, List.rev (resolvedId :: output))))
        end
      (* TODO: add more cases and make error message more specific *)
    | paramsToEnvList (srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "lambda: invalid parameter list"
          ))

  fun compileQuote (_, V.Pair(stx, V.Nil)) =
        OrError.return (A.Constant (V.stripStx stx))
    | compileQuote (srcloc, _) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "quote: invalid syntax"
          ))
  
  fun compileSyntax (_, V.Pair(stx, V.Nil)) =
        OrError.return (A.Constant stx)
    | compileSyntax (srcloc, _) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "syntax: invalid syntax"
          ))
 
  val lambdaName = S.fromString "lambda"
  val quoteName = S.fromString "quote"
  val syntaxName = S.fromString "syntax"
  val ifName = S.fromString "if"
  val beginName = S.fromString "begin"
  val letrecName = S.fromString "letrec"

  fun handleFreeId (srcloc, id) = let
    fun error msg = OrError.errorThunk (fn () => (locToString srcloc) ^ msg)
  in
    if S.=(lambdaName, id) then (
      error "lambda: invalid syntax"
    ) else if S.=(quoteName, id) then (
      error "quote: invalid syntax" 
    ) else if S.=(syntaxName, id) then (
      error "syntax: invalid syntax" 
    ) else if S.=(ifName, id) then (
      error "if: invalid syntax" 
    ) else if S.=(beginName, id) then (
      error "begin: invalid syntax" 
    ) else if S.=(letrecName, id) then (
      error "letrec: invalid syntax" 
    ) else (
      OrError.return (A.GlobalRef (I.lookupOrInsert (idMap, id)))
    )
  end


  fun compileExpr (env, V.Stx (V.Sym id, ctx, srcloc)) =
        let val resolvedId = LC.resolve (ctx, id, 0) in
          case CE.lookup (env, resolvedId) of
            CE.Free => handleFreeId (srcloc, resolvedId)
          | CE.Shallow i => OrError.return (A.ShallowRef i)
          | CE.Deep (i, j) => OrError.return (A.DeepRef (i, j))
        end

    | compileExpr (env, V.Stx (V.Pair(rator as V.Stx (V.Sym ratorId, ctx, _),
                                      args), 
                               _, srcloc)) =
        let val resolved_ratorId = LC.resolve (ctx, ratorId, 0) in
          if S.=(lambdaName, resolved_ratorId) then (
            compileLambda (srcloc, env, args)
      
          ) else if S.=(quoteName, resolved_ratorId) then (
            compileQuote (srcloc, args)
      
          ) else if S.=(syntaxName, resolved_ratorId) then (
            compileSyntax (srcloc, args)

          ) else if S.=(ifName, resolved_ratorId) then (
            compileIf (srcloc, env, args)

          ) else if S.=(beginName, resolved_ratorId) then (
            compileBegin (srcloc, env, args)

          ) else if S.=(letrecName, resolved_ratorId) then (
            compileLetrec (srcloc, env, args)

          ) else (
            compileApp (srcloc, env, rator, args)
          )
        end
    
    | compileExpr (env, V.Stx(e as V.True, _, _)) = 
        OrError.return (A.Constant e)
    | compileExpr (env, V.Stx(e as V.False, _, _)) = 
        OrError.return (A.Constant e)
    | compileExpr (env, V.Stx(e as V.Int _, _, _)) = 
        OrError.return (A.Constant e)
    | compileExpr (env, V.Stx(e as V.Char _, _, _)) = 
        OrError.return (A.Constant e)
    | compileExpr (env, V.Stx(e as V.Str _, _, _)) = 
        OrError.return (A.Constant e)
    
    | compileExpr (env, V.Stx (V.Pair(f, args), _, srcloc)) =
        compileApp (srcloc, env, f, args)
    
    | compileExpr (env, stx) =
        OrError.errorString "not implemented yet"


  and compileApp (srcloc, env, f, args) =
        compileExpr (env, f) >>= (fn compiled_f =>
        compileList ("application: bad syntax", srcloc, [],
                     env, args) >>= (fn compiled_args =>
        let val args_vec = Vector.fromList compiled_args in
          case compiled_f of
              A.FunFixArity (arity, body) =>
                if arity = Vector.length args_vec then (
                  OrError.return (A.Let (args_vec, body))
                ) else (
                  OrError.return (A.App (compiled_f, args_vec))
                )
            | _ => OrError.return (A.App (compiled_f, args_vec))
        end))
          

  and compileList (_, _, output, _, V.Nil) = 
        OrError.return output
    | compileList (msg, srcloc, output, env, V.Pair(hd, tl)) =
        compileExpr (env, hd) >>= (fn expr =>
        compileList (msg, srcloc, expr :: output, env, tl))
    | compileList (errorMsg, srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ errorMsg
            ))
 

  and compileLambda (srcloc, env, V.Pair(params, body as V.Pair(_,_))) =
        paramsToEnvList (srcloc, [], 0, params) >>= (fn idList =>
        case idList of
          FixArity (arity, ids) =>
            let val env = CE.extend (env, ids) in
              compileList ("lambda: invalid syntax", 
                           srcloc, [], env, body)
              >>= (fn compiledBody =>
                    OrError.return
                      (A.FunFixArity (arity, List.rev compiledBody)))
            end
        | VarArity (minArity, ids) =>
            let val env = CE.extend (env, ids) in
              compileList ("lambda: invalid syntax",
                           srcloc, [], env, body)
              >>= (fn compiledBody =>
                    OrError.return
                      (A.FunVarArity (minArity, 
                                      List.rev compiledBody)))
            end
        )
    | compileLambda (srcloc, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "lambda: invalid syntax"
            ))


  and extractBinding (_,
                      V.Stx(V.Pair(V.Stx(V.Sym id, ctx, id_srcloc),
                                   V.Pair(expr, V.Nil)), _, _),
                      ids) =
        let val resolvedId = LC.resolve (ctx, id, 0) in
          checkDuplicate (resolvedId, id_srcloc, ids) >>= (fn () =>
          OrError.return (resolvedId, expr))
        end
    | extractBinding (srcloc, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax (not an identifier and expression for a binding)"
            ))
        

  and extractBindings (_, ids, exprs, V.Nil) = 
        OrError.return (List.rev ids, exprs)
    | extractBindings (srcloc, ids, exprs, V.Pair(binding, rest)) =
        extractBinding (srcloc, binding, ids) >>= (fn (id, expr) =>
        extractBindings (srcloc, id :: ids, expr :: exprs, rest))
    | extractBindings (srcloc, _, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax"
            ))

  and compileLetrecExprs (env, output, []) = OrError.return output
    | compileLetrecExprs (env, output, hd :: tl) =
        compileExpr (env, hd) >>= (fn compiled_expr =>
        compileLetrecExprs (env, compiled_expr :: output, tl))
 
  
  and compileLetrec (srcloc, env, V.Pair(V.Stx(bindings,_,_),
                                         body as V.Pair(_,_))) =
        extractBindings (srcloc, [], [],
                         bindings) >>= (fn (idList, exprList) =>
        let val env = CE.extend (env, idList) in
          compileLetrecExprs (env, [], exprList) >>= (fn compiledExprs =>
          compileList ("letrec: invalid syntax",
                       srcloc, [], env, body) >>= (fn compiledBody =>
          OrError.return (A.Letrec (Vector.fromList compiledExprs,
                                    List.rev compiledBody))))
        end)
    | compileLetrec (srcloc, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "letrec: invalid syntax"
            ))


  and compileIf (_, env, V.Pair(cond, V.Pair(thn, V.Pair(els, V.Nil)))) =
        compileExpr (env, cond) >>= (fn compiled_cond =>
        compileExpr (env, thn) >>= (fn compiled_then =>
        compileExpr (env, els) >>= (fn compiled_else =>
        OrError.return (A.If (compiled_cond, compiled_then, compiled_else)))))
    | compileIf (srcloc, _, _) =
        OrError.errorThunk (fn () =>
          ( (locToString srcloc)
          ^ "if: invalid syntax"
          ))


  and compileBegin (srcloc, env, body as V.Pair(_, _)) =
        compileList ("begin: invalid syntax",
                     srcloc, [], env, body)
        >>= (fn compiled_body =>
              OrError.return (A.Begin (List.rev compiled_body)))
    | compileBegin (srcloc, _, V.Nil) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "begin: empty form is not allowed"
            ))
    | compileBegin (srcloc, _, _) =
        OrError.errorThunk (fn () =>
            ( (locToString srcloc)
            ^ "begin: invalid syntax"
            ))
in
  compileExpr  (CE.empty, stx) >>= (fn ast =>
  OrError.return (ast, I.mkIndexToIdTable idMap))
end



end
