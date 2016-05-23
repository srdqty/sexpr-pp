val lexStream = Lexer.mkLexInstream(TextIO.getInstream(TextIO.stdIn), "stdin")

fun locToString {name, column, line, position, span} =
  "@(" ^ (Int.toString line) ^ "," ^ (Int.toString column) ^ 
  "," ^ (Int.toString position) ^ "," ^ (Int.toString span) ^ ")"

fun tokToString (Lexer.End srcloc) = "END" ^ (locToString srcloc)
  | tokToString (Lexer.Lparen srcloc) = "LPAREN" ^ (locToString srcloc)
  | tokToString (Lexer.Rparen srcloc) = "RPAREN" ^ (locToString srcloc)
  | tokToString (Lexer.Dot srcloc) = "DOT" ^ (locToString srcloc)
  | tokToString (Lexer.ExprComment srcloc) = "#;" ^ (locToString srcloc)
  | tokToString (Lexer.Char(c, srcloc)) = "CHAR(" ^ c ^ ")"^ (locToString srcloc)
  | tokToString (Lexer.Str(s, srcloc)) = "STR(" ^ (String.toString s) ^ ")" ^ (locToString srcloc)
  | tokToString (Lexer.NumBoolOrSym(a, srcloc)) = "NUMBOOLORSYM(" ^ a ^ ")" ^ (locToString srcloc)

structure Datum =
struct
  type symbol_table = unit
  type srcloc =
  { name : string,
    line : int, column : int, position : int, span : int }

  datatype datum =
    Nil
  | True
  | False
  | Pair of datum * datum
  | Undefined
  | Str of string
  | Sym of string

  type t = datum

  fun makeNil () = Nil
  fun makePair (a, b) = Pair (a, b)

  fun wrapWithSourceInfo (x, _) = x

  fun isNil Nil = true
    | isNil _ = false
  fun isPair(Pair _) = true
    | isPair _ = false

  fun parseNumBoolOrSym("#t", _, _) = OrError.return True
    | parseNumBoolOrSym("#true", _, _) = OrError.return True
    | parseNumBoolOrSym("#f", _, _) = OrError.return False
    | parseNumBoolOrSym("#false", _, _) = OrError.return False
    | parseNumBoolOrSym(sym, _, _) = OrError.return (Sym sym)

  fun parseCharConstant(s, _) = OrError.return (Str s)

  fun parseStringLiteral(s, _) = OrError.return (Str s)

  fun car (Pair (hd, _)) = hd
    | car _ = raise (Fail "car: not a pair")

  fun cdr (Pair (_, tl)) = tl
    | cdr _ = raise (Fail "cdr: not a pair")

  fun isStx _ = false

  fun isAtom Nil = true
    | isAtom True = true
    | isAtom False = true
    | isAtom (Str _) = true
    | isAtom (Sym(_)) = true
    | isAtom _ = false

  fun atomToString Nil = SOME "()"
    | atomToString True = SOME "#t"
    | atomToString False = SOME "f"
    | atomToString (Str s) = SOME s
    | atomToString (Sym(s)) = SOME s
    | atomToString _ = NONE

  fun writeStx(t, {writeString, writeDatum}) = writeDatum t
end

structure Reader = ReaderFn(structure DatumConstructor = Datum
                            structure Lexer = Lexer)
structure Writer = WriterFn(Datum)

val >>= = OrError.>>=
infix >>=

fun read_file() =
  Reader.read () lexStream >>= (fn datum =>
  case datum of
    NONE => (print "<eof>\n"; OrError.return ())
  | SOME e => (Writer.write print e; print "\n"; read_file()))

val () = 
  case read_file() of
    Result.Ok () => ()
  | Result.Error err => print ((Error.toString err) ^ "\n")
