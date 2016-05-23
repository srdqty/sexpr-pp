functor ReaderWriterDatumFn (Value : VALUE) =
struct


type datum = Value.t
type symbol_table = Value.SymbolTable.t

type srcloc = 
  {name : string,
   line : int, column : int, position : int, span : int}

structure V = Value
structure LC = V.LexicalContext

fun makeNil () = V.Nil
fun makePair (hd, tl) = V.Pair (hd, tl)

fun wrapWithSourceInfo (v, srcloc) = V.Stx (v, LC.empty, SOME srcloc)

fun isNil V.Nil = true
  | isNil _ = false

fun isPair (V.Pair _) = true
  | isPair _ = false

fun parseNumBoolOrSym ("#t", _, _) = OrError.return V.True
  | parseNumBoolOrSym ("#true", _, _) = OrError.return V.True
  | parseNumBoolOrSym ("#f", _, _) = OrError.return V.False
  | parseNumBoolOrSym ("#false", _, _) = OrError.return V.False
  | parseNumBoolOrSym (str, _, _) =
     (case Int.fromString str of
        SOME n => OrError.return (V.Int n)
      | NONE => OrError.return (V.Sym (V.SymbolTable.Symbol.fromString str)))

fun parseCharConstant ("#\\newline", _) = OrError.return (V.Char #"\n")
  | parseCharConstant ("#\\space", _) = OrError.return (V.Char #" ")
  | parseCharConstant ("#\\tab", _) = OrError.return (V.Char #"\t")
  | parseCharConstant (s, {name, line, column, position, span}) = 
      if (String.size s) = 3 then (
        OrError.return (Value.Char (String.sub (s, 2)))
      ) else (
        OrError.errorThunk (fn () =>
          ( name
          ^ ":" ^ (Int.toString line)
          ^ ":" ^ (Int.toString column)
          ^ ": invalid character constant"))
      )

fun parseStringLiteral (s, {name, line, column, position, span}) =
 (case String.fromString (String.substring (s, 1, (String.size s) - 1)) of
    NONE =>
      OrError.errorThunk (fn () =>
        ( name
        ^ ":" ^ (Int.toString line)
        ^ ":" ^ (Int.toString column)
        ^ ": invalid string literal"))
  | SOME s => OrError.return (Value.Str s))

type t = datum

fun isAtom V.Nil = true
  | isAtom V.True = true
  | isAtom V.False = true
  | isAtom V.Void = true
  | isAtom V.Undefined = true
  | isAtom (V.Int _) = true
  | isAtom (V.Char _) = true
  | isAtom (V.Sym _) = true
  | isAtom (V.Str _) = true
  | isAtom (V.Ref _) = true
  | isAtom (V.FunFixArity _) = true
  | isAtom (V.FunVarArity _) = true
  | isAtom (V.FunBuiltInFixArity _) = true
  | isAtom (V.FunBuiltInVarArity _) = true
  | isAtom (V.Error _) = true
  | isAtom (V.Pair _) = false
  | isAtom (V.Stx _) = false

fun isSyntax (V.Stx _) = true
  | isSyntax _ = false

fun car (V.Pair (hd, _)) = hd
  | car _ = raise (Fail "car: not a pair")

fun cdr (V.Pair (_, tl)) = tl
  | cdr _ = raise (Fail "cdr: not a pair")

fun writeInt (n, ws)  = 
  if n < 0 then (
      ws "-"
    ; writeInt (~n, ws)
  ) else (
    ws (Int.toString n)
  )

fun displayAtom (V.Nil, ws) = ws "()"
  | displayAtom (V.True, ws) = ws "#t"
  | displayAtom (V.False, ws) = ws "#f"
  | displayAtom (V.Void, ws) = ws "#<void>"
  | displayAtom (V.Undefined, ws) = ws "#<undefined>"
  | displayAtom (V.Int n, ws) = writeInt (n, ws) 
  | displayAtom (V.Char c, ws) = ws (String.str c)
  | displayAtom (V.Sym s, ws) = ws (V.SymbolTable.Symbol.toString s)
  | displayAtom (V.Str s, ws) = ws s
  | displayAtom (V.Ref _, ws) = ws "#<ref>"
  | displayAtom (V.FunFixArity _, ws) = ws "#<function>"
  | displayAtom (V.FunVarArity _, ws) = ws "#<function>"
  | displayAtom (V.FunBuiltInFixArity _, ws) = ws "#<function>"
  | displayAtom (V.FunBuiltInVarArity _, ws) = ws "#<function>"
  | displayAtom (V.Error _, ws) = ws "#<error>"
  | displayAtom (V.Pair _, _) =
      raise (Fail "ReaderWriterDatumFn.displayAtom: expected atom")
  | displayAtom (V.Stx _, _) =
      raise (Fail "ReaderWriterDatumFn.displayAtom: expected atom")

fun writeChar (#"\n", ws) = ws "#\\newline"
  | writeChar (#"\t", ws) = ws "#\\tab"
  | writeChar (#" ", ws) = ws "#\\space"
  | writeChar (c, ws) = ws ("#\\" ^ (String.str c))
fun writeAtom (V.Nil, ws) = ws "()"
  | writeAtom (V.True, ws) = ws "#t"
  | writeAtom (V.False, ws) = ws "#f"
  | writeAtom (V.Void, ws) = ws "#<void>"
  | writeAtom (V.Undefined, ws) = ws "#<undefined>"
  | writeAtom (V.Int n, ws) = writeInt (n, ws) 
  | writeAtom (V.Char c, ws) = writeChar (c, ws)
  | writeAtom (V.Sym s, ws) = ws (V.SymbolTable.Symbol.toString s)
  | writeAtom (V.Str s, ws) = (ws "\""; ws (String.toString s); ws "\"")
  | writeAtom (V.Ref _, ws) = ws "#<ref>"
  | writeAtom (V.FunFixArity _, ws) = ws "#<function>"
  | writeAtom (V.FunVarArity _, ws) = ws "#<function>"
  | writeAtom (V.FunBuiltInFixArity _, ws) = ws "#<function>"
  | writeAtom (V.FunBuiltInVarArity _, ws) = ws "#<function>"
  | writeAtom (V.Error _, ws) = ws "#<error>"
  | writeAtom (V.Pair _, _) =
      raise (Fail "ReaderWriterDatumFn.writeAtom: expected atom")
  | writeAtom (V.Stx _, _) =
      raise (Fail "ReaderWriterDatumFn.writeAtom: expected atom")

fun unWrapStx (V.Stx (v, _, _)) = v
  | unWrapStx v = v

fun writeSyntax (stx, {writeString, writeDatum}) =
  writeDatum (V.stripStx stx)

fun writeRenamedSyntax (V.Stx(V.Sym sym, ctx, _), {writeString, writeDatum}) =
      let
        val resolvedSym = V.LexicalContext.resolve (ctx, sym, 0)
      in
        writeString (V.SymbolTable.Symbol.toString resolvedSym)
      end
  | writeRenamedSyntax (V.Stx(datum, _, _), {writeString, writeDatum}) =
      writeDatum datum
  | writeRenamedSyntax _ =
      raise (Fail "ReaderWriterDatumFn.writeRenamedSyntax: expected syntax object")


end
