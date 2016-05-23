functor EvalFn
(
  structure Value : VALUE
  structure LinkTable : LINK_TABLE
  sharing type Value.t = LinkTable.Value.t
  sharing type Value.SymbolTable.Symbol.t
             = LinkTable.Value.SymbolTable.Symbol.t
) : EVAL =
struct

structure Value = Value
structure LinkTable = LinkTable

type ast = Value.t Value.Ast.t
type environment = Value.t Value.EvalEnv.t
type index_to_global_id_table = Value.SymbolTable.Symbol.t vector
type global_id_to_value_table = LinkTable.t
type phase = int
type mark = Value.LexicalContext.Mark.t
type expand_env = Value.t Value.ExpandEnv.t

structure V = Value
structure A = Value.Ast
structure Env = Value.EvalEnv
structure S = Value.SymbolTable.Symbol

local
  val >>= = OrError.>>=
  infix >>=

  
  fun eval (phase, expandEnv, mark, env, A.Constant v) = OrError.return v

    | eval (phase, expandEnv, mark, env, A.ShallowRef j) = Env.shallowRef (env, j)

    | eval (phase, expandEnv, mark, env, A.DeepRef (i, j)) = Env.deepRef (env, i, j)
    
    | eval (phase, expandEnv, mark, env, A.GlobalRef i) = Env.globalRef (env, i)

    | eval (phase, expandEnv, mark, env, A.If (cond, thn, els)) =
        eval (phase, expandEnv, mark, env, cond) >>= (fn condValue =>
       (case condValue of
          V.False => eval (phase, expandEnv, mark, env, els)
        | _ => eval (phase, expandEnv, mark, env, thn)))

    | eval (phase, expandEnv, mark, env, A.Begin body) = evalBody (phase, expandEnv, mark, env, body)

    | eval (phase, expandEnv, mark, env, A.FunFixArity (arity, body)) =
        OrError.return 
          (V.FunFixArity (arity, env, body))

    | eval (phase, expandEnv, mark, env, A.FunVarArity (minArity, body)) =
        OrError.return 
          (V.FunVarArity (minArity, env, body))
    
    | eval (phase, expandEnv, mark, env, A.Let (args, body)) =
        evalLetOrFixArgs (phase, expandEnv, mark, env, args) >>= (fn frame =>
        evalBody (phase, expandEnv, mark, Env.extend (env, frame), body))

    | eval (phase, expandEnv, mark, env, A.App (operator, args)) =
        eval (phase, expandEnv, mark, env, operator) >>= (fn evaledOperator =>
        case (Vector.length args, evaledOperator) of
          (argsCount, V.FunFixArity (arity, funEnv, body)) => 
            if argsCount = arity then (
              evalLetOrFixArgs (phase, expandEnv, mark, env, args) >>= (fn frame =>
              evalBody (phase, expandEnv, mark, Env.extend (funEnv, frame), body))
            ) else (
              OrError.errorThunk (fn () =>
                ( "eval: function expected "
                ^ (Int.toString arity)
                ^ " arguments, but got "
                ^ (Int.toString argsCount)
                ^ " arguments"))
            )
        | (argsCount, V.FunVarArity (minArity, funEnv, body)) =>
            if argsCount >= minArity then (
              evalVarArgs (phase, expandEnv, mark, minArity, env, args) >>= (fn frame =>
              evalBody (phase, expandEnv, mark, Env.extend (funEnv, frame), body))
            ) else (
              OrError.errorThunk (fn () =>
                ( "eval: function expected "
                ^ (Int.toString minArity)
                ^ " or more arguments, but got "
                ^ (Int.toString argsCount)
                ^ " arguments"))
            )
        | (argsCount, V.FunBuiltInFixArity (name, arity, f)) =>
            if argsCount = arity then (
              evalLetOrFixArgs (phase, expandEnv, mark, env, args) >>= (fn frame =>
              f (phase, expandEnv, mark, frame))
            ) else (
              OrError.errorThunk (fn () =>
                ( "eval: "
                ^ (S.toString name)
                ^ " expected "
                ^ (Int.toString arity)
                ^ " arguments, but got "
                ^ (Int.toString argsCount)
                ^ " arguments"))
            )
        | (argsCount, V.FunBuiltInVarArity (name, minArity, NONE, f)) =>
            if argsCount >= minArity then (
              evalVarArgs (phase, expandEnv, mark, minArity, env, args) >>= (fn frame =>
              f (phase, expandEnv, mark, frame))
            ) else (
              OrError.errorThunk (fn () =>
                ( "eval: "
                ^ (S.toString name)
                ^ " expected "
                ^ (Int.toString minArity)
                ^ " or more arguments, but got "
                ^ (Int.toString argsCount)
                ^ " arguments"))
            )
        | (argsCount, V.FunBuiltInVarArity (name, minArity, SOME maxArity, f)) =>
            if argsCount >= minArity andalso argsCount <= maxArity then (
              evalLetOrFixArgs (phase, expandEnv, mark, env, args) >>= (fn frame =>
              f (phase, expandEnv, mark, frame))
            ) else (
              OrError.errorThunk (fn () =>
                ( "eval: "
                ^ (S.toString name)
                ^ " expected "
                ^ (Int.toString minArity)
                ^ " to " ^ (Int.toString maxArity)
                ^ " arguments, but got "
                ^ (Int.toString argsCount)
                ^ " arguments"))
            )
        | _ => OrError.errorString "eval: applied operator is not a function")
    
    | eval (phase, expandEnv, mark, env, A.Letrec (args, body)) =
        evalLetRecArgs (phase, expandEnv, mark, env, args) >>= (fn newEnv =>
        evalBody (phase, expandEnv, mark, newEnv, body))


  and evalBody (phase, expandEnv, mark, env, []) = OrError.errorString "eval: empty list of expressions"
    | evalBody (phase, expandEnv, mark, env, expr :: []) = eval (phase, expandEnv, mark, env, expr)
    | evalBody (phase, expandEnv, mark, env, expr :: rest) = 
        eval (phase, expandEnv, mark, env, expr) >>= (fn _ =>
        evalBody (phase, expandEnv, mark, env, rest))

  (* 
  * Ast.Let and Ast.App arguments are stored in reverse order in the AST,
  * so use a fold right to iterate over the arguments.
  *)
  and evalLetOrFixArgs (phase, expandEnv, mark, env, args) = let
    val length = Vector.length args
    val frame = Array.array (length, V.Undefined)
    fun foldr i j =
      if i >= 0 then (
        eval (phase, expandEnv, mark, env, Vector.sub (args, i)) >>= (fn evaledArg =>
        ( Array.update (frame, j, evaledArg)
        ; foldr (i - 1) (j + 1)))
      ) else (
        OrError.return frame
      )
  in
    foldr (length - 1) 0
  end


  and evalVarArgs (phase, expandEnv, mark, minArity, env, args) = let
    val length = Vector.length args
    val frame = Array.array (minArity + 1, V.Undefined)
    (* Arguments are stored in reverse order, so use a version of sub
    *  that starts from the end of the args vector *)
    fun sub (vec, i) = Vector.sub (vec, length - 1 - i)
    fun evalRestArg (i, output) =
      if i < minArity then OrError.return output
      else (
        eval (phase, expandEnv, mark, env, sub (args, i)) >>= (fn evaledArg =>
        evalRestArg (i - 1, V.Pair(evaledArg, output)))
      )
    fun loop i =
      if i < minArity then (
        eval (phase, expandEnv, mark, env, sub (args, i)) >>= (fn evaledArg =>
        ( Array.update (frame, i, evaledArg)
        ; loop (i + 1)))
      ) else (
        evalRestArg (length - 1, V.Nil) >>= (fn rest =>
        ( Array.update (frame, minArity, rest)
        ; OrError.return frame))
      )
  in
    loop 0
  end


  and evalLetRecArgs (phase, expandEnv, mark, env, args) = let
    val length = Vector.length args
    val frame = Array.array (length, V.Undefined)
    val newEnv = Env.extend (env, frame)
    fun foldl i =
      if i < length then (
        eval (phase, expandEnv, mark, newEnv, Vector.sub (args, i)) >>= (fn evaledArg =>
        ( Array.update (frame, i, evaledArg)
        ; foldl (i + 1)))
      ) else (
        OrError.return newEnv
      )
  in
    foldl 0
  end

  fun makeFixFrame (name, arity, args) = let
    val frame = Array.fromList args
    val argsCount = Array.length frame
  in
    if argsCount = arity then (
      OrError.return frame
    ) else (
      OrError.errorThunk (fn () =>
      ( "apply: "
      ^ (S.toString name)
      ^ " expected "
      ^ (Int.toString arity)
      ^ " arguments, but got "
      ^ (Int.toString argsCount)
      ^ " arguments"))
    )
  end

  fun fillFrame (minArity, args) = let
    val frame = Array.array (minArity + 1, V.Undefined)
    fun loop (i, lst) =
      if i < minArity then (
        Array.update (frame, i, List.hd lst)
      ; loop (i + 1, List.tl lst)
      ) else (
        Array.update (frame, i, V.fromList lst)
      )
  in
    ( loop (0, args)
    ; frame)
  end

  fun makeVarFrame (name, minArity, args) = let
    val argsCount = List.length args
  in
    if argsCount >= minArity then (
      OrError.return (fillFrame (minArity, args))
    ) else (
      OrError.errorThunk (fn () =>
      ( "apply: "
      ^ (S.toString name)
      ^ " expected "
      ^ (Int.toString minArity)
      ^ " or more arguments, but got "
      ^ (Int.toString argsCount)
      ^ " arguments"))
    )
  end

  val fName = S.fromString "function"
  fun apply (phase, expandEnv, mark, V.FunFixArity (arity, funEnv, body), args) =
        makeFixFrame (fName, arity, args) >>= (fn frame =>
        evalBody (phase, expandEnv, mark, Env.extend (funEnv, frame), body))
    | apply (phase, expandEnv, mark, V.FunVarArity (minArity, funEnv, body), args) =
        makeVarFrame (fName, minArity, args) >>= (fn frame =>
        evalBody (phase, expandEnv, mark, Env.extend (funEnv, frame), body))
    | apply (phase, expandEnv, mark, V.FunBuiltInFixArity (name, arity, f), args) =
        makeFixFrame (name, arity, args) >>= (fn frame =>
        f (phase, expandEnv, mark, frame))
    | apply (phase, expandEnv, mark, V.FunBuiltInVarArity (name, minArity, NONE, f), args) =
        makeVarFrame (name, minArity, args) >>= (fn frame =>
        f (phase, expandEnv, mark, frame))
    | apply (phase, expandEnv, mark, 
             V.FunBuiltInVarArity (name, minArity, SOME maxArity, f),
             args) =
        let
          val frame = Array.fromList args
          val argsCount = Array.length frame
        in
          if argsCount >= minArity andalso argsCount <= maxArity then (
              f (phase, expandEnv, mark, frame)
          ) else (
            OrError.errorThunk (fn () =>
              ( "eval: "
              ^ (S.toString name)
              ^ " expected "
              ^ (Int.toString minArity)
              ^ " to " ^ (Int.toString maxArity)
              ^ " arguments, but got "
              ^ (Int.toString argsCount)
              ^ " arguments"))
          )
        end
    | apply (_, _, _, _, _) = 
        OrError.errorString "apply: applied operator is not a function"
in
  val eval = eval
  val apply = apply
end



fun link (symVec, linkTable) = let
  fun lookup sym =
    case LinkTable.lookup (linkTable, sym) of
      SOME value => value
    | NONE => V.Undefined (* should never happen, expander is supposed to
                             catch references to undefined identifiers *)
in
  Env.mkFromGlobalTable (Vector.map lookup symVec)
end


end
