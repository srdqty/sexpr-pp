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

val >>= = OrError.>>=
infix >>=

fun lex_file() =
  Lexer.readToken lexStream >>= (fn tok =>
  case tok of
    tok as (Lexer.End _)=> 
      ( print ((tokToString tok) ^ "\n")
      ; Result.ok_unit
      )
  | tok => 
      ( print ((tokToString tok) ^ "\n")
      ; lex_file()
      )
  )

val () =
  case lex_file() of
    Result.Ok () => ()
  | Result.Error err => print ((Error.toString err) ^ "\n")
