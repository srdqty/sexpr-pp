functor CoreFunctionsFn
(
  structure Value : VALUE
  structure InterpreterStateTypes : INTERPRETER_STATE_TYPES
  structure Eval : EVAL
  structure Writer : WRITER
  structure Expander : EXPANDER
  sharing type Value.t 
             = Eval.Value.t 
             = Writer.datum
             = InterpreterStateTypes.PhaseState.Value.t
             = Expander.Value.t
  sharing type Value.SymbolTable.Symbol.t
             = InterpreterStateTypes.PhaseState.Value.LexicalContext.Name.t
             = InterpreterStateTypes.PhaseState.ExpandEnv.Symbol.t
             = Expander.StopSet.Symbol.t
  sharing type Value.ExpandEnv.t
             = InterpreterStateTypes.PhaseState.ExpandEnv.t
             = Eval.Value.ExpandEnv.t
  sharing type Eval.Value.LexicalContext.Mark.t
             = Value.LexicalContext.Mark.t
  sharing type InterpreterStateTypes.t
             = Expander.InterpreterStateTypes.t
) : CORE_FUNCTIONS =
struct
  
  
structure Value = Value
structure InterpreterStateTypes = InterpreterStateTypes

structure V = Value
structure S = V.SymbolTable.Symbol
structure IST = InterpreterStateTypes
structure PS = IST.PhaseState
structure Tower = IST.Tower
structure E = PS.ExpandEnv
structure LC = V.LexicalContext
structure SS = Expander.StopSet

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

val >>= = OrError.>>=
infix >>=

fun mkCoreFunctions (iState as IST.State {exitRequested, ...}) = let
  val coreFunctions = 
  [ 
    let
      val name = S.fromString "exit"
      val arity = 0
      fun f (_, _, _, _) = 
        ( exitRequested := true
        ; OrError.return V.Void)
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end
  
    , let
      val name = S.fromString "display"
      val arity = 1
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        fun writeString s = print s
      in
        ( Writer.display writeString datum
        ; OrError.return V.Void)
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    , let
      val name = S.fromString "displayln"
      val arity = 1
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        fun writeString s = print s
      in
        ( Writer.display writeString datum
        ; writeString "\n"
        ; OrError.return V.Void)
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    , let
      val name = S.fromString "display-verbose"
      val arity = 1
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        fun writeString s = print s
      in
        ( Writer.displayVerbose writeString datum
        ; OrError.return V.Void)
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    , let
      val name = S.fromString "write"
      val arity = 1
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        fun writeString s = print s
      in
        ( Writer.display writeString datum
        ; OrError.return V.Void)
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    , let
      val name = S.fromString "write-renamed-syntax"
      val arity = 1
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        fun writeString s = print s
      in
        ( Writer.writeRenamedSyntax writeString datum
        ; OrError.return V.Void)
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end


  , let
      val name = S.fromString "apply"
      val minArity = 2
      fun f (phase, _, _, args) = let
        val func = Array.sub (args, 0)
        val firstArg = Array.sub (args, 1)
        val restArgs = Array.sub (args, 2)

        fun g (x, (lst, false)) = (x :: lst, false)
          | g (x, (_, true)) = (V.toList x, false)
        fun lastArg x = V.foldr g ([], true) x

        val argsList = 
              case restArgs of
                V.Nil => #1 (lastArg firstArg)
              | restArgs => firstArg :: (#1 (lastArg restArgs))
      in
        Eval.apply (phase, V.ExpandEnv.mkExpandEnv(), LC.Mark.null,
                    func, argsList)
      end
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end
  
  , let
      val name = S.fromString "error"
      val arity = 2
      fun f (_, _, _, frame) = let
        val context = Array.sub (frame, 0)
        val errorData = Array.sub (frame, 1)
      in
        case context of
          V.Stx (_, _, srcloc) => 
            OrError.return (V.Error (errorData, srcloc))
        | _ => 
            OrError.errorString
              "error: expected syntax argument in first position"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "error-line"
      val arity = 1
      fun f (_, _, _, frame) = let
        val error = Array.sub (frame, 0)
      in
        case error of
          V.Error (_, SOME {line, ...}) => 
            OrError.return (V.Int line)
        | V.Error (_, NONE) => OrError.return (V.False)
        | _ => 
            OrError.errorString
              "error-line: expected error value as argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "error-column"
      val arity = 1
      fun f (_, _, _, frame) = let
        val error = Array.sub (frame, 0)
      in
        case error of
          V.Error (_, SOME {column, ...}) => 
            OrError.return (V.Int column)
        | V.Error (_, NONE) => OrError.return (V.False)
        | _ => 
            OrError.errorString
              "error-column: expected error value as argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "error-data"
      val arity = 1
      fun f (_, _, _, frame) = let
        val error = Array.sub (frame, 0)
      in
        case error of
          V.Error (data, _) => OrError.return data
        | _ => 
            OrError.errorString
              "error-data: expected error value as argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    (* Constructors for values that can't be read in by a reader *)
  , let
      val name = S.fromString "void"
      val arity = 0
      fun f (_, _, _, _) = OrError.return V.Void
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "make-ref"
      val arity = 1
      fun f (_, _, _, frame) = OrError.return (V.Ref (ref (Array.sub (frame, 0))))
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    (* Datatype predicates *)
  , let
      val name = S.fromString "null?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Nil => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "false?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.False => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "void?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Void => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "integer?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Int _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "char?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Char _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "string?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Str _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "symbol?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Sym _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "identifier?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          (V.Stx (V.Sym _, _, _)) => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "ref?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Ref _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "pair?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Pair _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "list?"
      val arity = 1
      fun f (_, _, _, frame) =
        if V.isList (Array.sub (frame, 0)) then (
          OrError.return V.True
        ) else (
          OrError.return V.False
        )
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "syntax?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Stx _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "error?"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Error _ => OrError.return V.True
        | _ => OrError.return V.False
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end


    (* Operations on symbols *)
  , let
      val name = S.fromString "string->symbol"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Str str => OrError.return (V.Sym (S.fromString str))
        | _ => OrError.errorString "string->symbol: expected string argument"
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "symbol->string"
      val arity = 1
      fun f (_, _, _, frame) =
        case Array.sub (frame, 0) of
          V.Sym sym => OrError.return (V.Str (S.toString sym))
        | _ => OrError.errorString "symbol->string: expected symbol argument"
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "symbol=?"
      val arity = 2
      fun f (_, _, _, frame) = let
        val sym1 = Array.sub (frame, 0)
        val sym2 = Array.sub (frame, 1)
      in
        case (sym1, sym2) of
          (V.Sym sym1, V.Sym sym2) =>  
            OrError.return (V.fromBool (S.=(sym1, sym2)))
        | _ => OrError.errorString "symbol=?: expected symbol arguments"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    
    (* Operations on integers *)
  , let
      val name = S.fromString "="
      val arity = 2
      fun f (_, _, _, frame) = let
        val n1 = Array.sub (frame, 0)
        val n2 = Array.sub (frame, 1)
      in
        case (n1, n2) of
          (V.Int n1, V.Int n2) =>  
            OrError.return (V.fromBool (n1 = n2))
        | _ => OrError.errorString "=: expected integer arguments"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "-"
      val minArity = 1
      fun f (_, _, _, frame) = let
        val first = Array.sub (frame, 0)
        val args = Array.sub (frame, 1)
        fun sub (x, accum) =
          accum >>= (fn accum =>
          case x of
            V.Int x => OrError.return (accum - x)
          | _ => OrError.errorString "-: expected integer arguments")
        fun subtract (n, args) =
          V.foldl sub (OrError.return n) args
      in
        case (first, args) of
          (V.Int n, V.Nil) => OrError.return (V.Int (~n))
        | (V.Int n, V.Pair _) => 
            subtract (n, args) >>= (fn result =>
            OrError.return (V.Int result))
        | _ => OrError.errorString "-: expected integer arguments"
      end
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end

  , let
      val name = S.fromString "+"
      val minArity = 0
      fun f (_, _, _, frame) = let
        val args = Array.sub (frame, 0)
        fun add (x, accum) =
          accum >>= (fn accum =>
          case x of
            V.Int x => OrError.return (x + accum)
          | _ => OrError.errorString "+: expected integer arguments")
        val sum = V.foldl add (OrError.return 0) args
      in
        sum >>= (fn sum =>
        OrError.return (V.Int sum))
      end
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end

  , let
      val name = S.fromString "*"
      val minArity = 0
      fun f (_, _, _, frame) = let
        val args = Array.sub (frame, 0)
        fun mult (x, accum) =
          accum >>= (fn accum =>
          case x of
            V.Int x => OrError.return (x * accum)
          | _ => OrError.errorString "*: expected integer arguments")
        val product = V.foldl mult (OrError.return 1) args
      in
        product >>= (fn product =>
        OrError.return (V.Int product))
      end
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end

(*
  , (name, minusFun)
  , (name, divideFun)
*)


    (* Operations on lists *)
  , let
      val name = S.fromString "list"
      val minArity = 0
      fun f (_, _, _, frame) = OrError.return (Array.sub (frame, 0))
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end

  , let
      val name = S.fromString "cons"
      val arity = 2
      fun f (_, _, _, frame) = let
        val car = Array.sub (frame, 0)
        val cdr = Array.sub (frame, 1)
      in
        OrError.return (V.Pair (car, cdr))
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "car"
      val arity = 1
      fun f (_, _, _, frame) = let
        val arg = Array.sub (frame, 0)
      in
        case arg of
          V.Pair(hd, _) => OrError.return hd
        | _ => OrError.errorString "car: expected pair argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end
  
   , let
      val name = S.fromString "length"
      val arity = 1
      fun f (_, _, _, frame) = let
        val arg = Array.sub (frame, 0)
      in
        case arg of
          V.Nil => OrError.return (V.Int 0)
        | V.Pair(_, _) => OrError.return (V.Int (V.length arg))
        | _ => OrError.errorString "length: expected list argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "cdr"
      val arity = 1
      fun f (_, _, _, frame) = let
        val arg = Array.sub (frame, 0)
      in
        case arg of
          V.Pair(_, tl) => OrError.return tl
        | _ => OrError.errorString "cdr: expected pair argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

(*
  , (name, mapFun)
  , (name, forEachFun)
  , (name, foldlFun)
  , (name, foldrFun)
*)

(*
    (* Operations on characters *)
*)
  , let
      val name = S.fromString "char->integer"
      val arity = 1
      fun f (_, _, _, frame) = let
        val arg = Array.sub (frame, 0)
      in
        case arg of
          V.Char c => OrError.return (V.Int (Char.ord c))
        | _ => OrError.errorString "char->integer: expected character argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "integer->char"
      val arity = 1
      fun f (_, _, _, frame) = let
        val arg = Array.sub (frame, 0)
      in
        case arg of
          V.Int n => OrError.return (V.Char (Char.chr n))
        | _ => OrError.errorString "integer->char: expected integer argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end


    (* Operations on strings *)
  , let
      val name = S.fromString "string-ref"
      val arity = 2
      fun f (_, _, _, frame) = let
        val str = Array.sub (frame, 0)
        val i = Array.sub (frame, 1)
      in
        case (str, i) of
          (V.Str str, V.Int i) =>  
            OrError.return (V.Char (String.sub (str, i)))
        | _ => OrError.errorString
                  "string-ref: expected a string and integer argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  
  , let
      val name = S.fromString "string-append"
      val minArity = 0
      fun f (_, _, _, frame) = let
        val args = Array.sub (frame, 0)
        fun add (s, accum) =
          accum >>= (fn accum =>
          case s of
            V.Str s => OrError.return (accum ^ s)
          | _ => OrError.errorString "string-append: expected string arguments")
        val str = V.foldl add (OrError.return "") args
      in
        str >>= (fn str =>
        OrError.return (V.Str str))
      end
    in
      (name, V.FunBuiltInVarArity (name, minArity, NONE, f))
    end


  (* Operations on references *)
  , let
      val name = S.fromString "ref-get"
      val arity = 1
      fun f (_, _, _, frame) = let
        val r = Array.sub (frame, 0)
      in
        case r of
          V.Ref r => OrError.return (!r)
        | _ => OrError.errorString "ref-get: expected ref argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "ref-set!"
      val arity = 2
      fun f (_, _, _, frame) = let
        val r = Array.sub (frame, 0)
        val value = Array.sub (frame, 1)
      in
        case r of
          V.Ref r => (r := value; OrError.return V.Void)
        | _ => OrError.errorString "ref-set!: expected ref argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

    
    (* Syntax and macro operations. *)
  , let
      val name = S.fromString "stx-e"
      val arity = 1
      fun f (_, _, _, frame) = let
        val stx = Array.sub (frame, 0)
      in
        case stx of
          V.Stx (x, _, _) => OrError.return x
        | _ => OrError.errorString "stx-e: expected syntax argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "mk-stx"
      val arity = 2
      fun f (_, _, _, frame) = let
        val datum = Array.sub (frame, 0)
        val stx = Array.sub (frame, 1)
      in
        case stx of
          V.Stx (x, ctx, _) => 
            OrError.return (V.Stx (datum, ctx, NONE))
        | _ => 
            OrError.errorString
              "mk-stx: expected syntax argument in second position"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "generate-ids"
      val arity = 2
      fun mkIds (0, str, ctx) = V.Nil
        | mkIds (n, str, ctx) = V.Pair(V.Stx (V.Sym (S.freshSym str), ctx, NONE),
                                  mkIds (n - 1, str, ctx))
      fun f (_, _, _, frame) = let
        val stx = Array.sub (frame, 0)
        val n = Array.sub (frame, 1)
      in
        case (n, stx) of
          (V.Int n, V.Stx (V.Sym sym, ctx, _)) => 
            if n >= 0 then (
              OrError.return (mkIds (n, S.toString sym, ctx))
            ) else (
            OrError.errorString
              "generate-ids: expected positive integer in second position"
            )
        | _ => 
            OrError.errorString
              "genearate-ids: expected identifier in first position and integer in second position"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "syntax-local-value"
      val arity = 1
      fun f (phase, expandEnv, _, frame) = let
        val stx = Array.sub (frame, 0)
      in
        case (phase, stx) of
          (0, _) => OrError.errorString
                      "syntax-local-value: not currently transforming"
        | (phase, V.Stx (V.Sym sym, ctx, _)) =>
            let 
              val resolvedId = LC.resolve (ctx, sym, phase-1)
            in
              case E.lookup (expandEnv, resolvedId) of
                SOME (E.Val v) => OrError.return v
              | _ => OrError.errorString
                        "syntax-local-value: not defined as syntax"
            end 
        | _ => OrError.errorString
                  "syntax-local-value: expected identifier argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  , let
      val name = S.fromString "local-expand"
      val arity = 2
      fun genStopSet (phase, stops) = let
        fun getStopId (V.Stx (V.Sym sym, ctx, _)) = 
              OrError.return (LC.resolve (ctx, sym, phase))
          | getStopId _ =
              OrError.errorString 
                "local-expand: expected identifier in stop list argument"
        fun getStopSet (stopSet, V.Nil) = OrError.return stopSet
          | getStopSet (stopSet, V.Pair (hd, tl)) =
              getStopId hd >>= (fn stopId =>
              getStopSet (SS.add (stopSet, stopId), tl))
          | getStopSet (_, _) =
              OrError.errorString
                "local-expand: expected identifer list as stop list argument"
      in
        getStopSet (SS.empty, stops)
      end
      fun markSyntax (stx, mark) =
            V.ctxMap (fn ctx => LC.mark (ctx, mark)) stx
      fun f (phase, expandEnv, mark, frame) = let
        val stx = Array.sub (frame, 0)
        val stops = Array.sub (frame, 1)
      in
        case (phase, stx) of
          (0, _) =>
            OrError.errorString "local-expand: not currently transforming"
        | (phase, stx as V.Stx (_, _, _)) =>
            genStopSet (phase-1, stops) >>= (fn stopSet =>
            markSyntax (stx, mark) >>= (fn markedSyntax =>
           (case Expander.expand (iState, phase-1, stopSet, markedSyntax) of
              Result.Error error => 
                OrError.return 
                  (V.Error (V.Str ("local-expand: "
                                  ^ Error.toString error),
                            NONE))
            | Result.Ok expandedStx =>
                markSyntax (expandedStx, mark))))
        | _ => 
            OrError.errorString
                "local-expand: expected syntax for first argument"
      end
    in
      (name, V.FunBuiltInFixArity (name, arity, f))
    end

  ]
in
  coreFunctions
end


end
